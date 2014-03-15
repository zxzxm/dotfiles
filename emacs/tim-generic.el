;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Generic and custom modes
;;----------------------------------------------------------------
;; Last modified: <2012-08-13 17:16:05 (thermans)>
;;----------------------------------------------------------------

(require 'generic)
(require 'generic-x)

;;{{{ LDIF Mode ..................................... &ldif ...

(define-generic-mode 'ldif-generic-mode
  '("#")
  nil
  '(("^dn:" . 'font-lock-warning-face)
    ("\\s$" . 'whitespace-highlight-face)
    ("^\\(.*\\):\\([^\n\r]*\\)$"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)))
  '("\\.ldif")
  (list
    (function
     (lambda ()
       (setq imenu-generic-expression
             '((nil "^\\[\\(dn:\\)\\]" 1)
               ("DN" "^\\s-*\\(.+\\)\\s-*=" 1)))
       )))
  "LDIF Editing Mode")
;;}}}

;;{{{ VCAL Mode ..................................... &vcal ...

(define-generic-mode 'ics-generic-mode
  nil
  '("ACTION"
    "ATTACH"
    "ATTENDEE"
    "CALSCALE"
    "CATEGORIES"
    "CLASS"
    "COMMENT"
    "COMPLETED"
    "CONTACT"
    "CREATED"
    "DESCRIPTION"
    "DTEND"
    "DTSTAMP"
    "DTSTART"
    "DUE"
    "DURATION"
    "EXDATE"
    "EXRULE"
    "FREEBUSY"
    "GEO"
    "LAST-MOD"
    "LOCATION"
    "METHOD"
    "ORGANIZER"
    "PERCENT"
    "PRIORITY"
    "PRODID"
    "RDATE"
    "RECURID"
    "RELATED"
    "REPEAT"
    "RESOURCES"
    "RRULE"
    "RSTATUS"
    "SEQ"
    "STATUS"
    "SUMMARY"
    "TRANSP"
    "TRIGGER"
    "TZID"
    "TZNAME"
    "TZOFFSETFROM"
    "TZOFFSETTO"
    "UID"
    "URL"
    "VERSION"
    "X-PROP")
  '(("BEGIN:" . 'font-lock-doc-string-face)
    ("END:" . 'font-lock-warning-face))
  '("\\.ics")
  nil
  "VCAL/ICS Mode")
;;}}}


;;{{{ Trace file for Yahoo IM  ...................... &yahoo ...
(define-generic-mode 'yahoo-generic-mode
  '("#")
  nil
  '(("^[A-Za-z].+" . 'font-lock-constant-face)
    ("^.+\)"  . 'font-lock-variable-name-face)
     ("[a-z]+.\:" . 'font-lock-keyword-face))
  nil
  nil
  "Yahoo IM Mode")
;;}}}

;;{{{ Openwave config.db file ....................... &config.db ...
(define-generic-mode 'configdb-generic-mode
  '("***")
  nil
  '(("^\\/\\(.+\\)\\/\\(.*\\)\\/\\(.*\\)\\: \\(\\.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-constant-face)))
  '("\\.db")
  nil
  "Config DB Mode")

;;}}}

;;{{{ TNSNames Mode ................................ &tnsnames ...
(define-generic-mode 'tnsnames-generic-mode
  '("#")
  '("DESCRIPTION"
    "ADDRESS_LIST"
    "ADDRESS"
    "PROTOCOL"
    "HOST"
    "PORT"
    "CONNECT_DATA")
  '(("^\\([A-Z].*\\)=" . 'font-lock-variable-name-face)
    ("\\(SID.*\\)=\\([A-Z].*\\))"
     (1 font-lock-constant-face)
     (2 font-lock-warning-face))
    ("\\(HOST.\\)=\\([A-Za-z0-9].*\\))"
     (2 font-lock-reference-face)))
    '("[Tt][Nn][Ss][Nn][Aa][Mm][Ee][Ss]\\.[Oo][Rr][Aa]")
    (list
     (function
     (lambda ()
       (setq imenu-generic-expression
             '((nil "^\\([A-Za-z].*\\)=" 1)))
       )))
"tnsnames.ora Editing Mode")

;;}}}

;;{{{ Snoop File (printed)................................ &snoop ...
(define-generic-mode 'snoop-generic-mode
  '("#")
  '("Ethernet II"
    "Internet Protocol"
    "User Datagram Protocol"
    "Session Initiation Protocol"
    "Session Description Protocol"
    )
'(("^\\(.*\\):\\([^\n\r]*\\)$"
     (1 font-lock-variable-name-face))
  ("Message Header" . font-lock-reference-face)
  ("^\\(Frame.*\\)" 1 font-lock-warning-face))
  '("\\.snp")
  nil
"Snoop File Mode")

;;}}}

;;{{{ Windows Registry .................................. &registry ...

(define-generic-mode 'w32-registry-generic-mode
  nil
  '("HKEY_CLASSES_ROOT"
    "HKEY_CURRENT_CONFIG"
    "HKEY_CURRENT_USER"
    "HKEY_LOCAL_MACHINE"
    "HKEY_USERS"
    )
  nil
  nil
  nil
"Win32 Registry Mode")
;;}}}

;;{{{ VIM resource file ................................ &vimrc ...
(define-generic-mode 'vimrc-generic-mode
  '("\"")
  '("set"
    "call"
    "filetype"
    "let"
    "colorscheme"
    "map"
    "nmap"
    "cmap"
    "vmap"
    "syntax"
    "runtime"
    "autocmd"
    "if"
    "execute"
    "else"
    "cab"
    "endif"
    "vnoremap"
    "nnoremap"
    "noremap"
    "cnoremap"
    )
  '(("\\(.*\\)=\\(.*\\)"
    (2 font-lock-variable-name-face)))
  nil
  nil
"VIM resource file mode")
;;}}}

;;{{{ VIM syntax file ................................ &vimsyntax ...
(define-generic-mode 'vimsyntax-generic-mode
  '("\"")
  '("Bell"
    "Boolean"
    "BufTabSelected"
    "CmdLine"
    "CmdCmdLine"
    "CmdModeMsg"
    "CmdInfoMsg"
    "CmdInput"
    "CmdMoreMsg"
    "CmdQuestion"
    "CmdPrompt"
    "CmdErrorMsg"
    "CmdWarningMsg"
    "CmdOutput"
    "CompDesc"
    "CompGroup"
    "CompIcon"
    "CompItem"
    "CompLess::after"
    "CompLess"
    "CompMore::after"
    "CompMore"
    "CompMsg"
    "CompResult"
    "CompTitle"
    "CompTitleSep"
    "Disabled"
    "EditorBlink1"
    "EditorBlink2"
    "EditorEditing"
    "EditorError"
    "Enabled"
    "ErrorMsg"
    "Filter"
    "FontCode"
    "FontMixed"
    "FontProportional"
    "FrameIndicator"
    "Function"
    "Gradient"
    "GradientLeft"
    "GradientRight"
    "Help"
    "HelpDefault"
    "HelpDescription"
    "HelpKeyword"
    "HelpKey"
    "HelpArg"
    "HelpBody"
    "HelpBorder"
    "HelpCode"
    "HelpDefault"
    "HelpEm"
    "HelpEx"
    "HelpString"
    "HelpExample"
    "HelpHead"
    "HelpHead1"
    "HelpHead2"
    "HelpHead3"
    "HelpInfo"
    "Hint"
    "HintActive"
    "HintElem"
    "HintImage"
    "Indicator"
    "InfoMsg"
    "Key"
    "Keyword"
    "LineNr"
    "Link"
    "LinkInfo"
    "Message"
    "ModeMsg"
    "MoreMsg"
    "NonText"
    "Normal"
    "Null"
    "Number"
    "Object"
    "Preview"
    "Question"
    "Search"
    "StatusCmdLine"
    "StatusInfoMsg"
    "StatusErrorMsg"
    "StatusWarningMsg"
    "StatusQuestion"
    "StatusModeMsg"
    "StatusMoreMsg"
    "StatusLine"
    "StatusLineNormal"
    "StatusLineBroken"
    "StatusLineExtended"
    "StatusLineSecure"
    "String"
    "TabClose"
    "TabIcon"
    "TabIconNumber"
    "TabNumber"
    "TabText"
    "Tag"
    "Title"
    "URL"
    "WarningMsg"
    )
  '(("^hi" . font-lock-constant-face)
    ("\\[.*\\]" . font-lock-variable-name-face)
    ("\\.*\\" . font-lock-type-face)
    )
  '(".vim\\'" ".penta\\'")
  nil
  "VIM syntax file mode")
;;}}}

;;{ MRTG Config............................................. &mrtg ...
(define-generic-mode 'mrtg-config-generic-mode
  '("#")
  '("Options"
    "WorkDir"
    "HtmlDir"
    "ImageDir"
    "LogDir"
    "Refresh"
    "Interval"
    "WriteExpires"
    "NoMib2"
    "SingleRequest"
    "SnmpOptions"
    "IconDir"
    "LoadMIBS"
    "Language"
    "LogFormat"
    "LibAdd"
    "PathAdd"
    "RunAsDaemon"
    )
  '( i("^i\\[Tt\\]arget" 1 'font-lock-variable-name-face))
  nil
  nil
)

;;{ Cisco Config............................................. &mrtg ...
(define-generic-mode 'cisco-config-generic-mode
  '("!")
  '("version"
    "boot-start-marker"
    "boot-end-marker"
    "hostname"
    "service"
    "logging"
    "enable"
    "username"
    "resource-pool"
    "clock"
    "calltracker"
    "aaa"
    "ip"
    "vpdn"
    "vpdn-group"
    "vpdn-template"
    "isdn"
    "modemcap"
    "voice"
    "ivr"
    "vxml"
    "http"
    "fax"
    "controller"
    "policy-map"
    "interface"
    "peer"
    "access-list"
    "dialer-list"
    "snmp-server"
    "radius-server"
    "call"
    "voice-port"
    "mgcp"
    "dial-peer"
    "privilege"
    "exception"
    "scheduler"
    "end"
    )
  '( i("^i\\[Tt\\]arget" 1 'font-lock-variable-name-face))
  nil
  nil
)


;;}}}
