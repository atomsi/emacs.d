;; === LLM配置变量 ===
(defvar my-ollama-server "115.159.204.60"
  "Ollama服务地址")
(defvar my-ollama-port 8443  ; 注意这里改为整数
  "Ollama服务端口")

(defvar llm-model "qwen3-coder:30b-a3b-q8_0"
  "代码专用LLM模型名称")

(defvar embedding-model "nomic-embed-text"
  "嵌入模型名称")


(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  ;; 语言设置
  (setopt ellama-language "Chinese")

  ;; 引入必要的依赖
  (require 'llm-ollama)

  ;; 定义基础provider配置，减少重复代码
  (defun my-llm-ollama (chat-model &optional embedding-model num-ctx)
    "创建标准的llm-ollama配置，减少重复代码。"
    (make-llm-ollama
     :chat-model chat-model
     :host my-ollama-server
     :port my-ollama-port
     :embedding-model (or embedding-model embedding-model)
     :default-chat-non-standard-params
     `(,(when num-ctx (cons "num_ctx" num-ctx))
       ;;,(when (string= chat-model llm-model) '("stop" . ("\n")))
       )))

  ;; 设置不同功能的provider
  (setopt ellama-provider
          (my-llm-ollama llm-model nil 40000))

  (setopt ellama-summarization-provider
          (my-llm-ollama llm-model nil 40000))

  (setopt ellama-coding-provider
          (my-llm-ollama llm-model nil 256000))

  ;; 会话命名设置
  (setopt ellama-naming-provider
          (my-llm-ollama llm-model nil nil))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)

  ;; 翻译功能设置
  (setopt ellama-translation-provider
          (my-llm-ollama llm-model nil 32768)) ;; 使用较小的上下文窗口提升速度

  ;; 提取功能设置
  (setopt ellama-extraction-provider
          (my-llm-ollama llm-model embedding-model 32768))

  ;; 自定义显示行为
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)

  :config
  ;; 在所有缓冲区的标题行显示ellama上下文和会话ID
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)

  ;; 处理滚动事件
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

(provide 'init-ellama)
;;; init-ellama.el ends here
