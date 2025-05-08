(ns floj.brainflow.brainflow-input-params
  (:require [floj.brainflow.board-ids :as id])
  (:import [brainflow BrainFlowInputParams IpProtocolTypes]))

(def ip-protocol-types
  {:no-ip-protocol IpProtocolTypes/NO_IP_PROTOCOL
   :udp IpProtocolTypes/UDP
   :tcp IpProtocolTypes/TCP})

(defn create-brainflow-input-params
  [& {:keys [ip-address ip-address-aux ip-address-anc mac-address
             serial-port ip-port ip-port-aux ip-port-anc ip-protocol
             other-info timeout serial-number file file-aux file-anc
             master-board]
      :or {serial-port "" mac-address "" ip-address ""
           ip-port 0 ip-port-aux 0 ip-port-anc 0
           ip-protocol :no-ip-protocol other-info "" timeout 0
           master-board :no-board}}]
  (let [input-params (BrainFlowInputParams.)
        ip-protocol-val (get ip-protocol-types ip-protocol)
        master-board-val (if (keyword? master-board)
                           (.get_code (id/board-ids-as-key master-board))
                           (int master-board))]
    (when-not (empty? ip-address)       (.set_ip_address input-params ip-address))
    (when-not (empty? ip-address-aux)   (.set_ip_address_aux input-params ip-address-aux))
    (when-not (empty? ip-address-anc)   (.set_ip_address_anc input-params ip-address-anc))
    (when-not (empty? mac-address)      (.set_mac_address input-params mac-address))
    (when-not (empty? serial-port)      (.set_serial_port input-params serial-port))

    (when (pos? ip-port)                (.set_ip_port input-params ip-port))
    (when (pos? ip-port-aux)            (.set_ip_port_aux input-params ip-port-aux))
    (when (pos? ip-port-anc)            (.set_ip_port_anc input-params ip-port-anc))

    (when ip-protocol-val               (.set_ip_protocol input-params (.get_code ip-protocol-val)))
    (when-not (empty? other-info)       (.set_other_info input-params other-info))
    (when (pos? timeout)                (.set_timeout input-params timeout))
    (when-not (empty? serial-number)    (.set_serial_number input-params serial-number))

    (when-not (empty? file)             (.set_file input-params file))
    (when-not (empty? file-aux)         (.set_file_aux input-params file-aux))
    (when-not (empty? file-anc)         (.set_file_anc input-params file-anc))

    (.set_master_board input-params master-board-val)
    input-params))

(defn set-ip-address!
  "Set the IP address for the board"
  [params ip-address]
  (.set_ip_address params ip-address)
  params)

(defn get-ip-address
  "Get the IP address for the board"
  [params]
  (.get_ip_address params))

(defn set-ip-address-aux!
  "Set the auxiliary IP address for the board"
  [params ip-address]
  (.set_ip_address_aux params ip-address)
  params)

(defn get-ip-address-aux
  "Get the auxiliary IP address for the board"
  [params]
  (.get_ip_address_aux params))

(defn set-ip-address-anc!
  "Set the anchor IP address for the board"
  [params ip-address]
  (.set_ip_address_anc params ip-address)
  params)

(defn get-ip-address-anc
  "Get the anchor IP address for the board"
  [params]
  (.get_ip_address_anc params))

(defn set-mac-address!
  "Set the MAC address for the board"
  [params mac-address]
  (.set_mac_address params mac-address)
  params)

(defn get-mac-address
  "Get the MAC address for the board"
  [params]
  (.get_mac_address params))

(defn set-serial-port!
  "Set the serial port for the board"
  [params serial-port]
  (.set_serial_port params serial-port)
  params)

(defn get-serial-port
  "Get the serial port for the board"
  [params]
  (.get_serial_port params))

(defn set-ip-port!
  "Set the IP port for the board"
  [params ip-port]
  (.set_ip_port params ip-port)
  params)

(defn get-ip-port
  "Get the IP port for the board"
  [params]
  (.get_ip_port params))

(defn set-ip-port-aux!
  "Set the auxiliary IP port for the board"
  [params ip-port]
  (.set_ip_port_aux params ip-port)
  params)

(defn get-ip-port-aux
  "Get the auxiliary IP port for the board"
  [params]
  (.get_ip_port_aux params))

(defn set-ip-port-anc!
  "Set the anchor IP port for the board"
  [params ip-port]
  (.set_ip_port_anc params ip-port)
  params)

(defn get-ip-port-anc
  "Get the anchor IP port for the board"
  [params]
  (.get_ip_port_anc params))

(defn set-ip-protocol!
  "Set the IP protocol for the board. Can accept either a keyword from ip-protocol-types 
   or the integer code directly"
  [params protocol]
  (if (keyword? protocol)
    (.set_ip_protocol params (get ip-protocol-types protocol))
    (.set_ip_protocol params protocol))
  params)

(defn get-ip-protocol
  "Get the IP protocol code for the board"
  [params]
  (.get_ip_protocol params))

(defn set-other-info!
  "Set other info for the board"
  [params other-info]
  (.set_other_info params other-info)
  params)

(defn get-other-info
  "Get other info for the board"
  [params]
  (.get_other_info params))

(defn set-timeout!
  "Set timeout for board operations"
  [params timeout]
  (.set_timeout params timeout)
  params)

(defn get-timeout
  "Get timeout for board operations"
  [params]
  (.get_timeout params))

(defn set-serial-number!
  "Set the serial number for the board"
  [params serial-number]
  (.set_serial_number params serial-number)
  params)

(defn get-serial-number
  "Get the serial number for the board"
  [params]
  (.get_serial_number params))

(defn set-file!
  "Set the file for the board"
  [params file]
  (.set_file params file)
  params)

(defn get-file
  "Get the file for the board"
  [params]
  (.get_file params))

(defn set-file-aux!
  "Set the auxiliary file for the board"
  [params file]
  (.set_file_aux params file)
  params)

(defn get-file-aux
  "Get the auxiliary file for the board"
  [params]
  (.get_file_aux params))

(defn set-file-anc!
  "Set the anchor file for the board"
  [params file]
  (.set_file_anc params file)
  params)

(defn get-file-anc
  "Get the anchor file for the board"
  [params]
  (.get_file_anc params))

(defn set-master-board!
  "Set the master board. Can accept either a keyword like :no-board or the integer code directly"
  [params board]
  (let [board-value (id/resolve-board board)]
    (.set_master_board params board-value))
  params)

(defn get-master-board
  "Get the master board code"
  [params]
  (.get_master_board params))

(defn to-json
  "Convert parameters to JSON string"
  [params]
  (.to_json params))