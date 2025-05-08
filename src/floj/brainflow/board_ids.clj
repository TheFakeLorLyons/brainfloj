(ns floj.brainflow.board-ids
  (:import [brainflow BoardIds]))

(def board-types
  {-100 "NO_BOARD"
   -3   "PLAYBACK_FILE_BOARD"
   -2   "STREAMING_BOARD"
   -1   "SYNTHETIC_BOARD"
   0   "CYTON_BOARD"
   1   "GANGLION_BOARD"
   2   "CYTON_DAISY_BOARD"
   3   "GALEA_BOARD"
   4   "GANGLION_WIFI_BOARD"
   5   "CYTON_WIFI_BOARD"
   6   "CYTON_DAISY_WIFI_BOARD"
   7   "BRAINBIT_BOARD"
   8   "UNICORN_BOARD"
   9   "CALLIBRI_EEG_BOARD"
   10   "CALLIBRI_EMG_BOARD"
   11   "CALLIBRI_ECG_BOARD"
   13   "NOTION_1_BOARD"
   14   "NOTION_2_BOARD"
   16   "GFORCE_PRO_BOARD"
   17   "FREEEEG32_BOARD"
   18   "BRAINBIT_BLED_BOARD"
   19   "GFORCE_DUAL_BOARD"
   20   "GALEA_SERIAL_BOARD"
   21   "MUSE_S_BLED_BOARD"
   22   "MUSE_2_BLED_BOARD"
   23   "CROWN_BOARD"
   24   "ANT_NEURO_EE_410_BOARD"
   25   "ANT_NEURO_EE_411_BOARD"
   26   "ANT_NEURO_EE_430_BOARD"
   27   "ANT_NEURO_EE_211_BOARD"
   28   "ANT_NEURO_EE_212_BOARD"
   29   "ANT_NEURO_EE_213_BOARD"
   30   "ANT_NEURO_EE_214_BOARD"
   31   "ANT_NEURO_EE_215_BOARD"
   32   "ANT_NEURO_EE_221_BOARD"
   33   "ANT_NEURO_EE_222_BOARD"
   34   "ANT_NEURO_EE_223_BOARD"
   35   "ANT_NEURO_EE_224_BOARD"
   36   "ANT_NEURO_EE_225_BOARD"
   37   "ENOPHONE_BOARD"
   38   "MUSE_2_BOARD"
   39   "MUSE_S_BOARD"
   40   "BRAINALIVE_BOARD"
   41   "MUSE_2016_BOARD"
   42   "MUSE_2016_BLED_BOARD"
   44   "EXPLORE_4_CHAN_BOARD"
   45   "EXPLORE_8_CHAN_BOARD"
   46   "GANGLION_NATIVE_BOARD"
   47   "EMOTIBIT_BOARD"
   48   "GALEA_BOARD_V4"
   49   "GALEA_SERIAL_BOARD_V4"
   50   "NTL_WIFI_BOARD"
   51   "ANT_NEURO_EE_511_BOARD"
   52   "FREEEEG128_BOARD"
   53   "AAVAA_V3_BOARD"
   54   "EXPLORE_PLUS_8_CHAN_BOARD"
   55   "EXPLORE_PLUS_32_CHAN_BOARD"
   56   "PIEEG_BOARD"})

(def board-ids-as-key
  {:no-board BoardIds/NO_BOARD
   :playback-file-board BoardIds/PLAYBACK_FILE_BOARD
   :streaming-board BoardIds/STREAMING_BOARD
   :synthetic-board BoardIds/SYNTHETIC_BOARD
   :cyton-board BoardIds/CYTON_BOARD
   :ganglion-board BoardIds/GANGLION_BOARD
   :cyton-daisy-board BoardIds/CYTON_DAISY_BOARD
   :galea-board BoardIds/GALEA_BOARD
   :ganglion-wifi-board BoardIds/GANGLION_WIFI_BOARD
   :cyton-wifi-board BoardIds/CYTON_WIFI_BOARD
   :cyton-daisy-wifi-board BoardIds/CYTON_DAISY_WIFI_BOARD
   :brainbit-board BoardIds/BRAINBIT_BOARD
   :unicorn-board BoardIds/UNICORN_BOARD
   :callibri-eeg-board BoardIds/CALLIBRI_EEG_BOARD
   :callibri-emg-board BoardIds/CALLIBRI_EMG_BOARD
   :callibri-ecg-board BoardIds/CALLIBRI_ECG_BOARD
   :notion-1-board BoardIds/NOTION_1_BOARD
   :notion-2-board BoardIds/NOTION_2_BOARD
   :gforce-pro-board BoardIds/GFORCE_PRO_BOARD
   :freeeeg32-board BoardIds/FREEEEG32_BOARD
   :brainbit-bled-board BoardIds/BRAINBIT_BLED_BOARD
   :gforce-dual-board BoardIds/GFORCE_DUAL_BOARD
   :galea-serial-board BoardIds/GALEA_SERIAL_BOARD
   :muse-s-bled-board BoardIds/MUSE_S_BLED_BOARD
   :muse-2-bled-board BoardIds/MUSE_2_BLED_BOARD
   :crown-board BoardIds/CROWN_BOARD
   :ant-neuro-ee-410-board BoardIds/ANT_NEURO_EE_410_BOARD
   :ant-neuro-ee-411-board BoardIds/ANT_NEURO_EE_411_BOARD
   :ant-neuro-ee-430-board BoardIds/ANT_NEURO_EE_430_BOARD
   :ant-neuro-ee-211-board BoardIds/ANT_NEURO_EE_211_BOARD
   :ant-neuro-ee-212-board BoardIds/ANT_NEURO_EE_212_BOARD
   :ant-neuro-ee-213-board BoardIds/ANT_NEURO_EE_213_BOARD
   :ant-neuro-ee-214-board BoardIds/ANT_NEURO_EE_214_BOARD
   :ant-neuro-ee-215-board BoardIds/ANT_NEURO_EE_215_BOARD
   :ant-neuro-ee-221-board BoardIds/ANT_NEURO_EE_221_BOARD
   :ant-neuro-ee-222-board BoardIds/ANT_NEURO_EE_222_BOARD
   :ant-neuro-ee-223-board BoardIds/ANT_NEURO_EE_223_BOARD
   :ant-neuro-ee-224-board BoardIds/ANT_NEURO_EE_224_BOARD
   :ant-neuro-ee-225-board BoardIds/ANT_NEURO_EE_225_BOARD
   :enophone-board BoardIds/ENOPHONE_BOARD
   :muse-2-board BoardIds/MUSE_2_BOARD
   :muse-s-board BoardIds/MUSE_S_BOARD
   :brainalive-board BoardIds/BRAINALIVE_BOARD
   :muse-2016-board BoardIds/MUSE_2016_BOARD
   :muse-2016-bled-board BoardIds/MUSE_2016_BLED_BOARD
   :explore-4-chan-board BoardIds/EXPLORE_4_CHAN_BOARD
   :explore-8-chan-board BoardIds/EXPLORE_8_CHAN_BOARD
   :ganglion-native-board BoardIds/GANGLION_NATIVE_BOARD
   :emotibit-board BoardIds/EMOTIBIT_BOARD
   :galea-board-v4 BoardIds/GALEA_BOARD_V4
   :galea-serial-board-v4 BoardIds/GALEA_SERIAL_BOARD_V4
   :ntl-wifi-board BoardIds/NTL_WIFI_BOARD
   :ant-neuro-ee-511-board BoardIds/ANT_NEURO_EE_511_BOARD
   :freeeeg128-board BoardIds/FREEEEG128_BOARD
   :aavaa-v3-board BoardIds/AAVAA_V3_BOARD
   :explore-plus-8-chan-board BoardIds/EXPLORE_PLUS_8_CHAN_BOARD
   :explore-plus-32-chan-board BoardIds/EXPLORE_PLUS_32_CHAN_BOARD
   :pieeg-board BoardIds/PIEEG_BOARD
   #_#_:neuropawn-knight-board BoardIds/NEUROPAWN_KNIGHT_BOARD
   #_#_:synchroni-trio-3-channels-board BoardIds/SYNCHRONI_TRIO_3_CHANNELS_BOARD
   #_#_:synchroni-octo-8-channels-board BoardIds/SYNCHRONI_OCTO_8_CHANNELS_BOARD
   #_#_:ob5000-8-channels-board BoardIds/OB5000_8_CHANNELS_BOARD
   #_#_:synchroni-pento-8-channels-board BoardIds/SYNCHRONI_PENTO_8_CHANNELS_BOARD
   #_#_:synchroni-uno-1-channels-board BoardIds/SYNCHRONI_UNO_1_CHANNELS_BOARD
   #_#_:ob3000-24-channels-board BoardIds/OB3000_24_CHANNELS_BOARD
   #_#_:biolistener-board BoardIds/BIOLISTENER_BOARD})

(def code->keyword
  "Map of integer codes to board-id keywords"
  {-100 :no-board
   -3 :playback-file-board
   -2 :streaming-board
   -1 :synthetic-board
   0 :cyton-board
   1 :ganglion-board
   2 :cyton-daisy-board
   3 :galea-board
   4 :ganglion-wifi-board
   5 :cyton-wifi-board
   6 :cyton-daisy-wifi-board
   7 :brainbit-board
   8 :unicorn-board
   9 :callibri-eeg-board
   10 :callibri-emg-board
   11 :callibri-ecg-board
   13 :notion-1-board
   14 :notion-2-board
   16 :gforce-pro-board
   17 :freeeeg32-board
   18 :brainbit-bled-board
   19 :gforce-dual-board
   20 :galea-serial-board
   21 :muse-s-bled-board
   22 :muse-2-bled-board
   23 :crown-board
   24 :ant-neuro-ee-410-board
   25 :ant-neuro-ee-411-board
   26 :ant-neuro-ee-430-board
   27 :ant-neuro-ee-211-board
   28 :ant-neuro-ee-212-board
   29 :ant-neuro-ee-213-board
   30 :ant-neuro-ee-214-board
   31 :ant-neuro-ee-215-board
   32 :ant-neuro-ee-221-board
   33 :ant-neuro-ee-222-board
   34 :ant-neuro-ee-223-board
   35 :ant-neuro-ee-224-board
   36 :ant-neuro-ee-225-board
   37 :enophone-board
   38 :muse-2-board
   39 :muse-s-board
   40 :brainalive-board
   41 :muse-2016-board
   42 :muse-2016-bled-board
   44 :explore-4-chan-board
   45 :explore-8-chan-board
   46 :ganglion-native-board
   47 :emotibit-board
   48 :galea-board-v4
   49 :galea-serial-board-v4
   50 :ntl-wifi-board
   51 :ant-neuro-ee-511-board
   52 :freeeeg128-board
   53 :aavaa-v3-board
   54 :explore-plus-8-chan-board
   55 :explore-plus-32-chan-board
   56 :pieeg-board})

(def string->keyword
  "Map of string names to board-id keywords"
  {"NO_BOARD" :no-board
   "PLAYBACK_FILE_BOARD" :playback-file-board
   "STREAMING_BOARD" :streaming-board
   "SYNTHETIC_BOARD" :synthetic-board
   "CYTON_BOARD" :cyton-board
   "GANGLION_BOARD" :ganglion-board
   "CYTON_DAISY_BOARD" :cyton-daisy-board
   "GALEA_BOARD" :galea-board
   "GANGLION_WIFI_BOARD" :ganglion-wifi-board
   "CYTON_WIFI_BOARD" :cyton-wifi-board
   "CYTON_DAISY_WIFI_BOARD" :cyton-daisy-wifi-board
   "BRAINBIT_BOARD" :brainbit-board
   "UNICORN_BOARD" :unicorn-board
   "CALLIBRI_EEG_BOARD" :callibri-eeg-board
   "CALLIBRI_EMG_BOARD" :callibri-emg-board
   "CALLIBRI_ECG_BOARD" :callibri-ecg-board
   "NOTION_1_BOARD" :notion-1-board
   "NOTION_2_BOARD" :notion-2-board
   "GFORCE_PRO_BOARD" :gforce-pro-board
   "FREEEEG32_BOARD" :freeeeg32-board
   "BRAINBIT_BLED_BOARD" :brainbit-bled-board
   "GFORCE_DUAL_BOARD" :gforce-dual-board
   "GALEA_SERIAL_BOARD" :galea-serial-board
   "MUSE_S_BLED_BOARD" :muse-s-bled-board
   "MUSE_2_BLED_BOARD" :muse-2-bled-board
   "CROWN_BOARD" :crown-board
   "ANT_NEURO_EE_410_BOARD" :ant-neuro-ee-410-board
   "ANT_NEURO_EE_411_BOARD" :ant-neuro-ee-411-board
   "ANT_NEURO_EE_430_BOARD" :ant-neuro-ee-430-board
   "ANT_NEURO_EE_211_BOARD" :ant-neuro-ee-211-board
   "ANT_NEURO_EE_212_BOARD" :ant-neuro-ee-212-board
   "ANT_NEURO_EE_213_BOARD" :ant-neuro-ee-213-board
   "ANT_NEURO_EE_214_BOARD" :ant-neuro-ee-214-board
   "ANT_NEURO_EE_215_BOARD" :ant-neuro-ee-215-board
   "ANT_NEURO_EE_221_BOARD" :ant-neuro-ee-221-board
   "ANT_NEURO_EE_222_BOARD" :ant-neuro-ee-222-board
   "ANT_NEURO_EE_223_BOARD" :ant-neuro-ee-223-board
   "ANT_NEURO_EE_224_BOARD" :ant-neuro-ee-224-board
   "ANT_NEURO_EE_225_BOARD" :ant-neuro-ee-225-board
   "ENOPHONE_BOARD" :enophone-board
   "MUSE_2_BOARD" :muse-2-board
   "MUSE_S_BOARD" :muse-s-board
   "BRAINALIVE_BOARD" :brainalive-board
   "MUSE_2016_BOARD" :muse-2016-board
   "MUSE_2016_BLED_BOARD" :muse-2016-bled-board
   "EXPLORE_4_CHAN_BOARD" :explore-4-chan-board
   "EXPLORE_8_CHAN_BOARD" :explore-8-chan-board
   "GANGLION_NATIVE_BOARD" :ganglion-native-board
   "EMOTIBIT_BOARD" :emotibit-board
   "GALEA_BOARD_V4" :galea-board-v4
   "GALEA_SERIAL_BOARD_V4" :galea-serial-board-v4
   "NTL_WIFI_BOARD" :ntl-wifi-board
   "ANT_NEURO_EE_511_BOARD" :ant-neuro-ee-511-board
   "FREEEEG128_BOARD" :freeeeg128-board
   "AAVAA_V3_BOARD" :aavaa-v3-board
   "EXPLORE_PLUS_8_CHAN_BOARD" :explore-plus-8-chan-board
   "EXPLORE_PLUS_32_CHAN_BOARD" :explore-plus-32-chan-board
   "PIEEG_BOARD" :pieeg-board})

(defn get-code
  "Get the integer code for a board type.
   Can accept either a keyword from the board-types map or a BoardIds enum directly."
  [board-type]
  (cond
    (keyword? board-type) (.get_code ^BoardIds (get board-types board-type))
    (instance? BoardIds board-type) (.get_code ^BoardIds board-type)
    (string? board-type) (.get_code ^BoardIds (get board-types (get string->keyword board-type)))
    :else (throw (IllegalArgumentException.
                  (str "Expected keyword, string, or BoardIds enum, got: " board-type)))))

(defn from-code
  "Get the BoardIds enum value from its integer code"
  [code]
  (BoardIds/from_code code))

(defn string-from-code
  "Get the string name of the BoardIds enum from its integer code"
  [code]
  (BoardIds/string_from_code code))

(defn keyword-from-code
  "Get the keyword representation of a BoardIds enum from its integer code"
  [code]
  (get code->keyword code))

(defn enum-value
  "Get the BoardIds enum value for the given board type.
   Can accept a keyword, string, integer code, or BoardIds enum."
  [board-type]
  (cond
    (keyword? board-type) (get board-types board-type)
    (integer? board-type) (from-code board-type)
    (string? board-type) (get board-types (get string->keyword board-type))
    (instance? BoardIds board-type) board-type
    :else (throw (IllegalArgumentException.
                  (str "Expected keyword, string, integer, or BoardIds enum, got: " board-type)))))

(defn resolve-board
  "Resolve a board identifier to a BoardIds object.
   Can accept either a keyword (like :no-board) or an integer code (like -100)."
  [board-id]
  (cond
    (keyword? board-id)
    (if-let [board-enum (get board-ids-as-key board-id)]
      board-enum
      (throw (IllegalArgumentException.
              (str "Unknown board keyword: " board-id
                   ". Available options are: " (keys board-ids-as-key)))))
    (number? board-id)
    board-id
    :else
    (throw (IllegalArgumentException.
            (str "Board identifier must be a keyword or integer, got: "
                 (type board-id))))))