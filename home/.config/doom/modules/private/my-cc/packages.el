;; -*- no-byte-compile: t; -*-
;;; private/my-cc/packages.el

(package! ccls :ignore t)
(package! clang-format)
(package! cmake-mode :recipe (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))
(package! llvm-mode :ignore t)
(package! modern-cpp-font-lock)
(package! tablegen-mode :ignore t)
