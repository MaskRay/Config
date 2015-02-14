# template

CHINA   = ...
SCHOOL  = ...
DEFAULT = ...

FactorOracle = (s) ->
  @oracle = new Array(s.length+1)
  pi = new Array(s.length+1)
  @oracle[0] = {}
  pi[0] = -1
  for c, i in s
    @oracle[i][c] = i+1
    k = pi[i]
    while k >= 0 and c not of @oracle[k]
      @oracle[k][c] = i+1
      k = pi[k]
    pi[i+1] = (if k == -1 then 0 else @oracle[k][c])
    @oracle[i+1] = {}
  @

FactorOracle::find = (s) ->
  k = 0
  for c in s
    k = @oracle[k][c]
    unless k?
      return false
  true

hosts_local = [
  ''
  'localhost'
  '127.0.0.1'
  '127.1'
  ''
]

hosts_school = [
  ''
  'example.com'
  ''
]

hosts_china = [
  ''
  'example.com'
  ''
]

oracle_local = new FactorOracle(hosts_local.join(','))
oracle_school = new FactorOracle(hosts_school.join(','))
oracle_china = new FactorOracle(hosts_china.join(','))

FindProxyForURL = (url, host) ->
  parts = host.replace(/:.*/,'').split '.'
  if parts.length > 2
    host = "#{parts[parts.length-2]}.#{parts[parts.length-1]}"
  key = ",#{host},"
  if oracle_local.find key
    'DIRECT'
  else if oracle_school.find key
    SCHOOL
  else if oracle_china.find key
    CHINA
  else
    DEFAULT
