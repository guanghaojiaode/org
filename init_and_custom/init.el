;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2006-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d
;; Version: 8.1.0
;; Keywords: .emacs.d centaur

;;
;;   CENTAUR EMACS - Enjoy Programming & Writing

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Centaur Emacs - A Fancy and Fast Emacs Configuration.
;;

;;; Code:

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Requisites
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-base)
(require 'init-hydra)

(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-snippet)

(require 'init-bookmark)
(require 'init-calendar)
(require 'init-dashboard)
(require 'init-dired)
(require 'init-highlight)
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
(require 'init-window)
(require 'init-treemacs)

(require 'init-eshell)
(require 'init-shell)

(require 'init-markdown)
(require 'init-org)
(require 'init-reader)

(require 'init-dict)
(require 'init-docker)
(require 'init-player)
(require 'init-utils)

;; Programming
(require 'init-vcs)
(require 'init-check)
(require 'init-lsp)
(require 'init-dap)

(require 'init-prog)
(require 'init-elisp)
(require 'init-c)
(require 'init-go)
(require 'init-rust)
(require 'init-python)
(require 'init-ruby)
(require 'init-elixir)
(require 'init-web)
(require 'org-ref)
(require 'org-ref-helm)
(require 'citar-org-roam)
(require 'citar-embark)
(citar-org-roam-mode 1)
(citar-embark-mode 1)
(add-hook 'org-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 'visual)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
;; Guanghao ;;
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(setq dictionary-server "dict.org")

;;; Language parsing

(use-package tree-sitter
  :ensure t
  :defer t
  :diminish " tree"
  :hook ((zig-mode-hook) . (lambda ()
			                 (tree-sitter-mode)
			                 (tree-sitter-hl-mode))))
(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package tree-sitter
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	    (treesit-install-language-grammar lang)
	    (message "`%s' parser was installed." lang)
	    (sit-for 0.75)))))

(use-package org-roam
  :ensure t ;; 自动安装
  :custom
  (require 'org-roam-dailies)  ;; 启用日记功能
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/org/") ;; 默认笔记目录, 提前手动创建好
  (org-roam-dailies-directory "~/org/dailies/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle) ;; 显示后链窗口
         ("C-c n u" . org-roam-ui-mode)
         ("C-c n d" . org-roam-dailies-find-date) ;; 日记菜单
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ) ;; 浏览器中可视化
  :config
  (org-roam-setup)
  (org-roam-bibtex-mode +1)
  (org-roam-db-autosync-mode))

(defun org-config-fill-prefix ()
  "Set `fill-prefix' to the empty string."
  (setq fill-prefix ""))
(add-hook 'org-mode-hook #'org-config-fill-prefix)
                                        ;以上用于创建org roam新文件时lsp的错误

;; 第一步: 告诉 Emacs 从哪里读取 Zotero 的信息
(setq zot_bib '("~/org/mylib/bib/mylib.bib") ; Zotero 用 Better BibTeX 导出的 .bib 文件. 可以是多个文件
      zot_pdf "~/org/mylib/pdf/" ; Zotero 的 ZotFile 同步文件夹
      org_refs "~/org/ref/" ) ; 自定义的 org-roam 文献笔记目录. 我的 org-roam 根目录是 ~/repos/notes

;; 第二步: 让 helm-bibtex 读取 Zotero 的信息
(use-package helm-bibtex ; 这里也可以用 ivy-bibtex 替换 helm-bibtex
  :ensure t
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

(setq org-cite-follow-processor 'helm-bibtex-org-cite-follow)

(setq bibtex-completion-display-formats
      '((article       . "${=has-pdf=:1}${=has-note=:3} ${=type=:10} ${year:4} {author:20} ${title:40} ${journal:20}")
        (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:10} ${year:4} ${author:20} ${title:40} Chapter ${chapter:20}")
        (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:10} ${year:4} ${author:20} ${title:40} ${booktitle:20}")
        (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:10} ${year:4} ${author:20} ${title:40} ${booktitle:20}")
        (t             . "${=has-pdf=:1}${=has-note=:3} ${=type=:10} ${year:4} ${author:20} ${title:40} ${journal:20}")))
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")
;;当一个文献条目有多个pdf附件时候会提示你选择打开哪一个
(setq bibtex-completion-find-additional-pdfs t)
;;;;Grammarly;;;;

(setq bibtex-completion-pdf-field "File")

(use-package lsp-grammarly
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-grammarly)
                       (lsp))))  ; or lsp-deferred

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(setq org-agenda-files (quote ("~/org/")))
(custom-set-variables
 '(org-directory "~/org/")
 '(org-agenda-files (list org-directory)))

(use-package citar
  :custom
  (citar-bibliography '("~/org/mylib/bib/mylib.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/org/mylib/bib/mylib.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c n b" . #'org-cite-insert)))

(setq citar-templates
      '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
        (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
        (note . "Notes on ${author editor:%etal}, ${title}")))

(setq citar-at-point-function 'embark-act)

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-actions))
  :custom
  (orb-insert-interface 'helm-bibtex) ; 与上面 helm-bibtex/ivy-bibtex 的选择保持一致
  (orb-insert-link-description 'citekey) ; 默认是用标题, 但是论文的标题一般很长, 不适合作为笔记链接的名字
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

                                        ;(setq org-roam-capture-templates
                                        ;'(;("d" "default" plain
                                        ;  "%?"
                                        ;  :target
                                        ;  (file+head
                                        ;   "%<%Y%m%d%H%M%S>-${slug}.org"
                                        ;   "#+title: ${note-title}\n")
                                        ;  :unnarrowed t)
                                        ;("n" "literature note" plain
                                        ; "%?"
                                        ; :target
                                        ; (file+head
                                        ;  "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${citar-citekey}.org"
                                        ;  "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n#+last_modified: %U\n\n")
                                        ; :unnarrowed t)))

(setq citar-org-roam-note-title-template "${author} - ${title}")
                                        ;(setq citar-org-roam-capture-template-key "n")

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

                                        ;(setq reftex-cite-format 'natbib)
;; Org mode export to PDF using pandoc

(setq org-ref-insert-cite-function
      (lambda ()
	    (org-cite-insert nil)))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(require 'ox)
(require 'ox-pandoc)
(setq org-pandoc-options '((standalone . t)
                           (pdf-engine . "pdflatex")))
(setq org-latex-prefer-user-labels t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;org noter;;;;;;;;;;;;;;;
(use-package org-noter
  :ensure t
  :custom
  (org-noter-notes-search-path '("~/org/mylib/pdf/")) ;; 默认笔记路径
  (org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
  (org-noter-max-short-selected-text-length 20) ;; 默认为 80
  (org-noter-default-heading-title "Notes of page $p$")) ;; 默认短标题格式
                                        ;:bind
                                        ;(("C-c n n" . org-noter) ;; 与 org-roam 配合
                                        ;:map org-noter-doc-mode-map ;; 加入左手键位
                                        ;("e" . org-noter-insert-note)
                                        ;("M-e" . org-noter-insert-precise-note)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; (define-key org-mode-map (kbd "C-c n b") 'org-ref-cite-insert-helm)
(define-key org-mode-map (kbd "C-c n o") 'citar-open-note)
(define-key org-mode-map (kbd "C-c n r") 'citar-dwim)
(define-key org-mode-map (kbd "C-c n SPC") 'flyspell-goto-next-error)
(define-key org-mode-map (kbd "C-c n p") 'citar-open-notes)
(define-key org-mode-map (kbd "C-c n n") 'citar-create-note)
(define-key org-mode-map (kbd "C-c n s") 'dictionary-search)
(define-key org-mode-map (kbd "C-c n e") 'lsp-execute-code-action)
;; Guanghao ;;
