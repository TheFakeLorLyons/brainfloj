(ns pong.main-menu
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]))

(e/defn main-menu-button [text on-click]
  (dom/div
   (dom/props {:class "main-menu-button"})
   (dom/on "click" (e/fn [] (on-click)))
   (dom/text text)))

(e/defn MainScreen [actions]
  (dom/div
   (dom/props {:class "main-screen"})

   ;; Game title
   (dom/div
    (dom/props {:class "main-screen-title"})
    (dom/h1 (dom/text "BCI PONG"))
    (dom/p (dom/text "Control the game using your brain waves!")))

   ;; Main menu
   (dom/div
    (dom/props {:class "main-menu"})

    ;; Start game button
    (main-menu-button "Start Game" (:start-game actions))

    ;; BCI setup button
    (main-menu-button "BCI Setup" (:show-bci-setup actions))

    ;; Settings button
    (main-menu-button "Settings" (:show-settings actions))

    ;; Instructions button
    (main-menu-button "How to Play" (:show-instructions actions))

    ;; Exit button (only works if not in browser)
    (main-menu-button "Exit" (:exit-game actions)))))

(e/defn InstructionsScreen [go-back]
  (dom/div
   (dom/props {:class "instructions-screen"})

   (dom/h2 (dom/text "How to Play BCI Pong"))

   (dom/div
    (dom/props {:class "instructions-content"})

    (dom/h3 (dom/text "Setup"))
    (dom/p (dom/text "1. Connect your BCI device using the BCI Setup menu"))
    (dom/p (dom/text "2. Train your 'up' and 'down' brain wave patterns"))
    (dom/p (dom/text "3. Start the game when ready!"))

    (dom/h3 (dom/text "Gameplay"))
    (dom/p (dom/text "• Control your paddle (left side) using your brain waves"))
    (dom/p (dom/text "• Think 'up' to move the paddle up"))
    (dom/p (dom/text "• Think 'down' to move the paddle down"))
    (dom/p (dom/text "• Score by getting the ball past the AI paddle"))
    (dom/p (dom/text "• First to 10 points wins!"))

    (dom/h3 (dom/text "Keyboard Controls"))
    (dom/p (dom/text "• Up/Down arrows: Manual paddle control"))
    (dom/p (dom/text "• Space: Pause/Resume game"))
    (dom/p (dom/text "• Esc: Return to main menu")))

   (dom/div
    (dom/props {:class "back-button"})
    (dom/on "click" (e/fn [_] (go-back)))
    (dom/text "Back to Main Menu"))))

(e/defn SettingsScreen [settings update-settings go-back]
  (e/client
   (let [current-settings (e/watch settings)]
     (dom/div
      (dom/props {:class "settings-screen"})

      (dom/h2 (dom/text "Game Settings"))

      (dom/div
       (dom/props {:class "settings-content"})

       ;; Game difficulty
       (dom/div
        (dom/props {:class "setting-item"})
        (dom/label (dom/text "Game Difficulty:"))
        (dom/select
         (dom/props {:value (:difficulty current-settings)})
         (dom/on "change" (e/fn [e]
                            (update-settings :difficulty (.. e -target -value))))
         (dom/option (dom/props {:value "easy"}) (dom/text "Easy"))
         (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
         (dom/option (dom/props {:value "hard"}) (dom/text "Hard"))))

       ;; Paddle size
       (dom/div
        (dom/props {:class "setting-item"})
        (dom/label (dom/text "Paddle Size:"))
        (dom/select
         (dom/props {:value (:paddle-size current-settings)})
         (dom/on "change" (e/fn [e]
                            (update-settings :paddle-size (.. e -target -value))))
         (dom/option (dom/props {:value "small"}) (dom/text "Small"))
         (dom/option (dom/props {:value "medium"}) (dom/text "Medium"))
         (dom/option (dom/props {:value "large"}) (dom/text "Large"))))

       ;; Game speed
       (dom/div
        (dom/props {:class "setting-item"})
        (dom/label (dom/text "Game Speed:"))
        (dom/select
         (dom/props {:value (:game-speed current-settings)})
         (dom/on "change" (e/fn [e]
                            (update-settings :game-speed (.. e -target -value))))
         (dom/option (dom/props {:value "slow"}) (dom/text "Slow"))
         (dom/option (dom/props {:value "normal"}) (dom/text "Normal"))
         (dom/option (dom/props {:value "fast"}) (dom/text "Fast"))))

       ;; BCI confidence threshold
       (dom/div
        (dom/props {:class "setting-item"})
        (dom/label (dom/text "BCI Confidence Threshold:"))
        (dom/input
         (dom/props {:type "range"
                     :min "0.3"
                     :max "0.9"
                     :step "0.05"
                     :value (:bci-threshold current-settings)})
         (dom/on "input" (e/fn [e]
                           (update-settings :bci-threshold (.. e -target -value)))))
        (dom/span (dom/text (str (:bci-threshold current-settings))))))

      (dom/div
       (dom/props {:class "back-button"})
       (dom/on "click" (e/fn [_] (go-back)))
       (dom/text "Back to Main Menu"))))))