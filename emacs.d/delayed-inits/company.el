(setq company-show-numbers t)
(setq company-backends '(company-bbdb
                         company-nxml
                         company-css
                         company-eclim
                         company-semantic
                         company-xcode
                         company-cmake
                         company-capf
                         company-files
                         (company-gtags company-keywords)
                         company-dabbrev))
