(defvar my-cquery-blacklist nil
  "List of paths that should not enable lsp-cquery")

(defvar my-cquery-whitelist '("Dev/llvm")
  "List of paths that should enable lsp-cquery. Takes priority over my-cquery-blacklist")
