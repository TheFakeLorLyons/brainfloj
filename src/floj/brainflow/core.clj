(ns floj.brainflow.core
  (:require [floj.brainflow.boardshim :as shim]
            [floj.brainflow.boardids :as id])
    (:import [brainflow BrainFlowInputParams]))

(defn get-board-id
  [board-id]
  (id/board-types board-id))

(defn initialize-board
  "Initialize a board with the given parameters"
  [board-id & {:keys [serial-port mac-address ip-address ip-port ip-protocol
                      other-info timeout master-board]
               :or {serial-port "" mac-address "" ip-address ""
                    ip-port 0 ip-protocol 0 other-info ""
                    timeout 0 master-board -1}}]
  (let [params (doto (BrainFlowInputParams.)
                 (.set_serial_port serial-port)
                 (.set_mac_address mac-address)
                 (.set_ip_address ip-address)
                 (.set_ip_port ip-port)
                 (.set_ip_protocol ip-protocol)
                 (.set_other_info other-info)
                 (.set_timeout timeout)
                 (.set_master_board master-board))]
    (shim/naive-board-shim-constructor board-id params)))