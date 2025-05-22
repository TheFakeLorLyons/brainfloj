(ns floj.bluetooth
  (:require [clojure.string :as str]
            [floj.api :as api]
            [floj.state :as state])
  (:import [com.sun.jna Native Memory]))

(gen-interface
  :name floj.bluetooth.SimpleBLELibrary
  :extends [com.sun.jna.Library]
  :methods [[simpleble_adapter_get_count [] int]
            [simpleble_adapter_get_handle [int] com.sun.jna.Pointer]
            [simpleble_adapter_scan_for [com.sun.jna.Pointer int] int]
            [simpleble_adapter_scan_get_results_count [com.sun.jna.Pointer] int]
            [simpleble_adapter_scan_get_results_handle [com.sun.jna.Pointer int] com.sun.jna.Pointer]
            [simpleble_adapter_release_handle [com.sun.jna.Pointer] void]
            [simpleble_peripheral_address [com.sun.jna.Pointer com.sun.jna.Pointer] com.sun.jna.Pointer]
            [simpleble_peripheral_identifier [com.sun.jna.Pointer com.sun.jna.Pointer] com.sun.jna.Pointer]
            [simpleble_peripheral_rssi [com.sun.jna.Pointer com.sun.jna.Pointer] short]
            [simpleble_peripheral_connect [com.sun.jna.Pointer] int]
            [simpleble_peripheral_release_handle [com.sun.jna.Pointer] void]])

(defn init
  "Initialize the SimpleBLE library"
  []
  (println "Loading SimpleBLE library...")
  (try
    (Native/loadLibrary "simpleble-c" floj.bluetooth.SimpleBLELibrary)
    (catch Exception e
      (println "Error loading SimpleBLE library:" (.getMessage e))
      nil)))

(defn initialize-bluetooth!
  "Initialize the SimpleBLE library"
  []
  (println "Initializing Bluetooth capabilities...")
  (when-not (:simpleble @state/state)
    (state/register-fn! :simpleble (atom nil)))
  (reset! state/simpleble (init)))

(defn bci-device?
  "Check if device is likely a BCI device based on name/address patterns"
  [device]
  (let [name (clojure.string/lower-case (:name device))]
    (or (re-find #"ganglion|neurosity|muse|openbci|brain|neuro|eeg" name)
      (and (not (nil? name))
        (not= name "")
        (not= name "unknown device")))))

(defn read-c-string
  [func peripheral]
  (let [buffer (Memory. 512)]
    (try
      (func peripheral buffer)
      (let [str (.getString buffer 0 "UTF-8")]
        (if (or (nil? str) (= str "")) "Unknown Device" str))
      (catch Exception e
        (println "Error reading C string:" (.getMessage e))
        "Unknown Device"))))

(defn get-rssi
  "Get RSSI value from peripheral"
  [simpleble peripheral]
  (let [rssi-ptr (Memory. 4)]
    (try
      (.simpleble_peripheral_rssi simpleble peripheral)
      (.getInt rssi-ptr 0)
      (catch Exception e
        (println "Error getting RSSI:" (.getMessage e))
        -100))))

(defn scan-devices
  "Scan for Bluetooth devices and return a sequence of maps with device info"
  []
  (let [simpleble @state/simpleble
        adapter-count (.simpleble_adapter_get_count simpleble)]
    (println "Adapters found:" adapter-count)
    (if (pos? adapter-count)
      (let [adapter (.simpleble_adapter_get_handle simpleble 0)
            _ (println "Starting scan...")
            _ (.simpleble_adapter_scan_for simpleble adapter 5000)
            device-count (.simpleble_adapter_scan_get_results_count simpleble adapter)
            devices (atom [])]
        (println "Devices found:" device-count)
        (doseq [i (range device-count)]
          (try
            (let [peripheral (.simpleble_adapter_scan_get_results_handle simpleble adapter i)
                  address (read-c-string #(.simpleble_peripheral_address simpleble %1 %2) peripheral)
                  name (read-c-string #(.simpleble_peripheral_identifier simpleble %1 %2) peripheral)
                  rssi (get-rssi simpleble peripheral)]
              (println (format "Device %d: %s (%s), RSSI: %d" i name address rssi))
              (swap! devices conj {:index i
                                   :name name
                                   :address address
                                   :rssi rssi})
              (.simpleble_peripheral_release_handle simpleble peripheral))
            (catch Exception e
              (println "Error processing device" i ":" (.getMessage e)))))
        (.simpleble_adapter_release_handle simpleble adapter)
        @devices)
      (do
        (println "No Bluetooth adapters found")
        []))))

(defn connect-to-device
  "Connect to a device by address"
  [address]
  (let [simpleble @state/simpleble
        adapter-count (.simpleble_adapter_get_count simpleble)]
    (when (pos? adapter-count)
      (let [adapter (.simpleble_adapter_get_handle simpleble 0)
            _ (.simpleble_adapter_scan_for simpleble adapter 5000)
            device-count (.simpleble_adapter_scan_get_results_count simpleble adapter)]
        (loop [i 0]
          (when (< i device-count)
            (let [peripheral (.simpleble_adapter_scan_get_results_handle simpleble adapter i)
                  dev-address (read-c-string #(.simpleble_peripheral_address simpleble %1 %2) peripheral)]
              (if (= dev-address address)
                (do
                  (println "Connecting to device:" address)
                  (let [result (.simpleble_peripheral_connect simpleble peripheral)]
                    (if (zero? result)
                      (do
                        (println "Connected successfully")
                        {:peripheral peripheral :adapter adapter})
                      (do
                        (println "Connection failed with error code:" result)
                        (.simpleble_peripheral_release_handle simpleble peripheral)
                        nil))))
                (do
                  (.simpleble_peripheral_release_handle simpleble peripheral)
                  (recur (inc i)))))))))))

(defn display-devices-cli
  "Display discovered devices in CLI and prompt for selection"
  [devices]
  (let [bci-devices (filter bci-device? devices)
        all-devices (concat bci-devices
                      (filter #(not (bci-device? %)) devices))]

    (println "\nDiscovered devices:")
    (println "-------------------")

    (doseq [[idx device] (map-indexed vector all-devices)]
      (let [bci-marker (if (bci-device? device) "[BCI] " "      ")
            device-name (if (empty? (:name device)) "Unknown Device" (:name device))]
        (printf "%d. %s%s (%s) - Signal: %d dBm\n"
          (inc idx)
          bci-marker
          device-name
          (:address device)
          (:rssi device))))

    (println "\nEnter the number of the device you want to connect to (or 0 to cancel):")
    (flush)

    (try
      (let [selection (Integer/parseInt (read-line))]
        (if (and (> selection 0) (<= selection (count all-devices)))
          (nth all-devices (dec selection))
          nil))
      (catch NumberFormatException _
        (println "Invalid selection. Please enter a number.")
        nil))))

(defn bluetooth-connection!
  "CLI function to scan for BCI devices and connect to selected one"
  [_]  ; Added parameter to match keybinding function signature
  (println "Initializing BCI device scan...")

  ; Make sure bluetooth is initialized
  (initialize-bluetooth!)

  (println "Scanning for BLE devices (this may take a few seconds)...")
  (let [devices (scan-devices)]
    (if (empty? devices)
      (println "No devices found. Please make sure your BCI device is powered on and in pairing mode.")
      (let [selected-device (display-devices-cli devices)]
        (if selected-device
          (do
            (println "Selected device:" (:name selected-device) "at" (:address selected-device))
            (println "Connecting...")
            (let [connection (connect-to-device (:address selected-device))]
              (if connection
                (do
                  (println "Direct BLE connection established to" (:name selected-device))
                  (api/connect! (:address selected-device) nil :connection connection))
                (println "Failed to establish BLE connection. Please try again."))))
          (println "No device selected, connection cancelled"))))))

(defn initialize-bluetooth-module!
  "Initialize the Bluetooth module for CLI use"
  []
  (println "Initializing Bluetooth module...")
  (state/register-fn! :bluetooth-connection! bluetooth-connection!)
  (initialize-bluetooth!))