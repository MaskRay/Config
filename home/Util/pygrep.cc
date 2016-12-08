#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
#include <codecvt>
#include <getopt.h>
#include <err.h>
#include <fstream>
#include <ftw.h>
#include <iostream>
#include <locale>
#include <re2/re2.h>
#include <stdint.h>
#include <sstream>
#include <string>
#include <sys/stat.h>
#include <sysexits.h>
#include <unordered_map>
#include <vector>
using namespace std;

int8_t opt_with_filename = -1, opt_files_matches;
int n_matches;
bool opt_line_number, opt_only, opt_quiet;
RE2* re;
unordered_map<char32_t, string> m;
wstring_convert<codecvt_utf8<char32_t>, char32_t> cvt;

int process(const char* fpath, const struct stat* sb, int typeflag, struct FTW* ftwbuf)
{
  if (typeflag != FTW_F) return 0;
  ifstream f(fpath);
  string line;
  int lineno = 0, local_matches = 0;
  while (getline(f, line)) {
    u32string utf32;
    lineno++;
    try {
      utf32 = cvt.from_bytes(line.data());
    } catch (range_error&) {
      return 0;
      break;
    }
    string py;
    vector<size_t> offsets;
    vector<pair<size_t, size_t>> matches;
    for (char32_t c: utf32) {
      auto it = m.find(c);
      if (it == m.end())
        py += cvt.to_bytes(c);
      else
        py += it->second;
      offsets.push_back(py.size());
    }
    size_t i = 0;
    for (size_t start = 0; start < py.size(); ) {
      re2::StringPiece res;
      if (! re->Match(py, start, py.size(), re2::RE2::Anchor::UNANCHORED, &res, 1)) break;
      for (; i < offsets.size() && offsets[i] <= res.data()-py.data(); i++);
      start = res.data()-py.data()+res.size();
      auto j = i;
      for (; i < offsets.size() && offsets[i] <= start; i++);
      if (j < i)
        matches.emplace_back(j, i);
    }
    if (matches.size()) {
      n_matches++;
      local_matches++;
      i = 0;
      if (! opt_quiet && ! opt_files_matches) {
        for (auto& x: matches) {
          if (opt_with_filename)
            printf("%s:", fpath);
          if (opt_line_number)
            printf("%d:", lineno);
          if (! opt_only)
            printf("%s", cvt.to_bytes(utf32.substr(i, x.first-i)).c_str());
          printf("\033[1;31m%s\033[m", cvt.to_bytes(utf32.substr(x.first, x.second-x.first)).c_str());
          if (opt_only)
            puts("");
          i = x.second;
        }
        if (! opt_only)
          printf("%s\n", cvt.to_bytes(utf32.substr(i)).c_str());
      }
    }
  }
  if (opt_files_matches && (opt_files_matches > 0) == (local_matches > 0))
    printf("%s\n", fpath);
  return 0;
}

int main(int argc, char* argv[])
{
  int opt;
  static struct option long_options[] = {
    {"help",                no_argument,       0,   1},
    {"files-without-match", no_argument,       0,   'L'},
    {"files-with-matches",  no_argument,       0,   'l'},
    {"line-number",         no_argument,       0,   'n'},
    {"no-filename",         no_argument,       0,   'h'},
    {"only-matching",       no_argument,       0,   'o'},
    {"with-filename",       no_argument,       0,   'H'},
    {0,                   0,                 0,   0},
  };

  while ((opt = getopt_long(argc, argv, "HhLlnoq", long_options, NULL)) != -1) {
    switch (opt) {
    case 1:
      errx(0, "pygrep pattern file...");
    case 'H':
      opt_with_filename = 1;
      break;
    case 'h':
      opt_with_filename = 0;
      break;
    case 'L':
      opt_files_matches = -1;
      break;
    case 'l':
      opt_files_matches = 1;
      break;
    case 'n':
      opt_line_number = true;
      break;
    case 'o':
      opt_only = true;
      break;
    case 'q':
      opt_quiet = true;
      break;
    }
  }
  if (argc-optind < 2)
    errx(EX_USAGE, "pygrep pattern file...");
  if (opt_with_filename < 0)
    opt_with_filename = argc-optind > 2;

  ifstream f("/home/ray/Information/词典/zidian_zhzh-kfcd-2013122.txt");
  string line, jt, ft, py;
  long lineno = 0;
  for (; getline(f, line); lineno++) {
    if (lineno >= 8) {
      istringstream iss(line);
      getline(iss, jt, '\t');
      getline(iss, ft, '\t');
      getline(iss, py);
      m[cvt.from_bytes(jt.data())[0]] = m[cvt.from_bytes(ft.data())[0]] = py.substr(0, py.size()-1);
    }
  }

  re = new RE2(argv[optind++]);
  for (; optind < argc; optind++) {
    struct stat sb;
    if (stat(argv[optind], &sb))
      err(EX_OSERR, "stat");
    nftw(argv[optind], process, 10, FTW_F);
  }
  delete re;
  return ! n_matches;
}
