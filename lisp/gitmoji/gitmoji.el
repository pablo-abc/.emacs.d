;;; gitmoji.el --- a gitmoji lookup tool

;;; commentary:

;;; Needs emojify to display
;;; Requires ivy to work

;;; code:
(require 'ivy)
(require 'magit)
(defvar gitmoji-emojis)
(defvar gitmoji-commit-msg)
(defvar gitmoji-ask-ticket)
(setq gitmoji-ask-ticket nil)
(setq gitmoji-commit-msg "")
(setq gitmoji-emojis
      '(("Improving structure/format of code" ":art:")
        ("Improving performance" ":zap:")
        ("Removing code or files" ":fire:")
        ("Fixing a bug" ":bug:")
        ("Critical hotfix" ":ambulance:")
        ("Introducing new features" ":sparkles:")
        ("Writing docs" ":pencil:")
        ("Deploying stuff" ":rocket:")
        ("Updating UI and style files" ":lipstick:")
        ("Initial commit" ":tada:")
        ("Adding tests" ":white_check_mark:")
        ("Fixing security issues" ":lock:")
        ("Fixing something on macOS" ":apple:")
        ("Fixing something on Linux" ":penguin:")
        ("Fixing something on Windows" ":checkered_flag:")
        ("Fixing something on Android" ":robot:")
        ("Fixing something on iOS" ":green_apple:")
        ("Removing linter warnings" ":rotating_light:")
        ("Work in progress" ":construction:")
        ("Fixing CI Build" ":green_heart:")
        ("Downgrading dependencies" ":arrow_down:")
        ("Upgrading dependencies" ":arrow_up:")
        ("Pinning dependencies to specific versions" ":pushpin:")
        ("Adding CI build system" ":construction_worker:")
        ("Adding analytics or tracking code" ":chart_with_upwards_trend:")
        ("Refactoring code" ":recycle:")
        ("Work about Docker" ":whale:")
        ("Adding a dependency" ":heavy_plus_sign:")
        ("Removing a dependency" ":heavy_minus_sign:")
        ("Changing configuration files" ":wrench:")
        ("Internationalization and localization" ":globe_with_meridians:")
        ("Fixing typos" ":pencil2:")
        ("Writing bad code that needs to be improved" ":poop:")
        ("Reverting changes" ":rewind:")
        ("Merging branches" ":twisted_rightwards_arrows:")
        ("Updating compiled files or packages" ":package:")
        ("Updating code due to external API changes" ":alien:")
        ("Moving or renaming files" ":truck:")
        ("Adding or updating license" ":page_facing_up:")
        ("Introducing breaking changes" ":boom:")
        ("Adding or updating assets" ":bento:")
        ("Updating code due to code review changes" ":ok_hand:")
        ("Improving accessibility" ":wheelchair:")
        ("Documenting source code" ":bulb:")
        ("Writing code drunkenly" ":beer:")
        ("Updating text and literals" ":speech_balloon:")
        ("Performing database related changes" ":card_box:")
        ("Adding logs" ":loud_sound:")
        ("Removing logs" ":mute:")
        ("Adding contributor(s)" ":busts_in_silhouette:")
        ("Improving user experience/usability" ":children_crossing:")
        ("Making architectural changes" ":building_construction:")
        ("Working on responsive design" ":iphone:")
        ("Mocking things" ":clown_face:")
        ("Adding an easter egg" ":egg:")
        ("Adding or updating a .gitignore file" ":see_no_evil:")
        ("Adding or updating snapshots" ":camera_with_flash:")
        ("Experimenting new things" ":alembic:")
        ("Improving SEO" ":mag:")
        ))

(when gitmoji-emojis
  (defun gitmoji-apropos-emoji ()
    "Insert the emoji specified."
    (interactive)
    (ivy-read "Look for emoji: "
              (seq-map (lambda (y) (concat (first y) " - " (second y))) gitmoji-emojis)
              :preselect 0
              :require-match t
              :action (with-ivy-window
                        (lambda (x)
                          (insert (second (assoc (first (split-string x " - ")) gitmoji-emojis)))))
              :caller 'gitmoji-apropos-emoji
              )
    ))

(defun gitmoji-add-emoji-to-commit ()
  "Append emoji to commit message."
  (ivy-read "Look for emoji: "
            (append '("None") (seq-map (lambda (y) (concat (first y) " - " (second y))) gitmoji-emojis))
            :preselect "None"
            :require-match t
            :action (lambda (x)
                      (let ((emojis (assoc (first (split-string x " - ")) gitmoji-emojis)))
                        (when emojis (setq gitmoji-commit-msg (concat gitmoji-commit-msg (second emojis))))
                        )
                      )
            :caller 'gitmoji-apropos-emoji
            )
  )

(defun gitmoji-add-ticket-to-commit ()
  "Ask for ticket identificator and add it to commit message."
  (let ((ticket (read-string "Enter ticket: ")) (msg-length (length gitmoji-commit-msg)))
    (while (< (- git-commit-summary-max-length msg-length) (+ (length ticket) 2))
      (setq ticket (read-string "Ticket too long, please enter another: "))
      )
    (when (> (length ticket) 0) (setq gitmoji-commit-msg (concat gitmoji-commit-msg (if (> msg-length 0) " " "") "[" ticket "]")))
    )
  )

(defun gitmoji-add-msg-to-commit ()
  "Ask for commit message."
  (let ((msg-length (length gitmoji-commit-msg)) (msg (read-string "Enter a commit message: ")))
    (while (< (- git-commit-summary-max-length msg-length) (length msg))
      (setq msg (read-string "Message too long, please enter another: "))
      )
    (setq gitmoji-commit-msg (concat gitmoji-commit-msg (if (> (length gitmoji-commit-msg) 0) " " "") msg))
    )
  )

(defun gitmoji-compose-commit ()
  "Compose commit message using gitmoji."
  (interactive)
  (setq gitmoji-commit-msg "")
  (gitmoji-add-emoji-to-commit)
  (when gitmoji-ask-ticket (gitmoji-add-ticket-to-commit))
  (gitmoji-add-msg-to-commit)
  (insert gitmoji-commit-msg)
  )

(provide 'gitmoji)
;;; gitmoji.el ends here
