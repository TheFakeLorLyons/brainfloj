(ns floj-test.brainflow-test.brainflow-input-params-test
  (:require [clojure.test :refer :all]
            [floj.brainflow.brainflow-input-params :as params])
  (:import [brainflow BrainFlowInputParams IpProtocolTypes BoardIds]))

;Mock paramas object
(defn- create-test-params []
  (-> (params/create-brainflow-input-params)
      (params/set-ip-address! "192.168.1.100")
      (params/set-ip-address-aux! "192.168.1.101")
      (params/set-ip-address-anc! "192.168.1.102")
      (params/set-mac-address! "AA:BB:CC:DD:EE:FF")
      (params/set-serial-port! "/dev/ttyUSB0")
      (params/set-ip-port! 7789)
      (params/set-ip-port-aux! 7790)
      (params/set-ip-port-anc! 7791)
      (params/set-ip-protocol! :tcp)
      (params/set-other-info! "test-info")
      (params/set-timeout! 5000)
      (params/set-serial-number! "SN12345")
      (params/set-file! "test-file.txt")
      (params/set-file-aux! "test-file-aux.txt")
      (params/set-file-anc! "test-file-anc.txt")
      (params/set-master-board! :no-board)))

;Test constructor
(deftest create-brain-flow-input-params-test
  (testing "Creating a new BrainFlowInputParams instance"
    (let [p (params/create-brainflow-input-params)]
      (is (instance? BrainFlowInputParams p))
      (is (= "" (params/get-ip-address p)))
      (is (= "" (params/get-ip-address-aux p)))
      (is (= "" (params/get-ip-address-anc p)))
      (is (= "" (params/get-mac-address p)))
      (is (= "" (params/get-serial-port p)))
      (is (= 0 (params/get-ip-port p)))
      (is (= 0 (params/get-ip-port-aux p)))
      (is (= 0 (params/get-ip-port-anc p)))
      (is (= (.get_code IpProtocolTypes/NO_IP_PROTOCOL) (params/get-ip-protocol p)))
      (is (= "" (params/get-other-info p)))
      (is (= 0 (params/get-timeout p)))
      (is (= "" (params/get-serial-number p)))
      (is (= "" (params/get-file p)))
      (is (= "" (params/get-file-aux p)))
      (is (= "" (params/get-file-anc p)))
      (is (= (.get_code BoardIds/NO_BOARD) (params/get-master-board p))))))

(deftest ip-address-test
  (testing "Setting and getting IP address"
    (let [p (params/create-brainflow-input-params)
          test-ip "192.168.1.100"]
      (params/set-ip-address! p test-ip)
      (is (= test-ip (params/get-ip-address p)))))

  (testing "Setting and getting auxiliary IP address"
    (let [p (params/create-brainflow-input-params)
          test-ip "192.168.1.101"]
      (params/set-ip-address-aux! p test-ip)
      (is (= test-ip (params/get-ip-address-aux p)))))

  (testing "Setting and getting anchor IP address"
    (let [p (params/create-brainflow-input-params)
          test-ip "192.168.1.102"]
      (params/set-ip-address-anc! p test-ip)
      (is (= test-ip (params/get-ip-address-anc p))))))

(deftest mac-address-test
  (testing "Setting and getting MAC address"
    (let [p (params/create-brainflow-input-params)
          test-mac "AA:BB:CC:DD:EE:FF"]
      (params/set-mac-address! p test-mac)
      (is (= test-mac (params/get-mac-address p))))))

(deftest serial-port-test
  (testing "Setting and getting serial port"
    (let [p (params/create-brainflow-input-params)
          test-port "/dev/ttyUSB0"]
      (params/set-serial-port! p test-port)
      (is (= test-port (params/get-serial-port p))))))

(deftest ip-port-test
  (testing "Setting and getting IP port"
    (let [p (params/create-brainflow-input-params)
          test-port 7789]
      (params/set-ip-port! p test-port)
      (is (= test-port (params/get-ip-port p)))))

  (testing "Setting and getting auxiliary IP port"
    (let [p (params/create-brainflow-input-params)
          test-port 7790]
      (params/set-ip-port-aux! p test-port)
      (is (= test-port (params/get-ip-port-aux p)))))

  (testing "Setting and getting anchor IP port"
    (let [p (params/create-brainflow-input-params)
          test-port 7791]
      (params/set-ip-port-anc! p test-port)
      (is (= test-port (params/get-ip-port-anc p))))))

(deftest ip-protocol-test
  (testing "Setting and getting IP protocol using keyword"
    (let [p (params/create-brainflow-input-params)]
      (params/set-ip-protocol! p :tcp)
      (is (= (.get_code IpProtocolTypes/TCP) (params/get-ip-protocol p)))))

  (testing "Setting and getting IP protocol using integer code"
    (let [p (params/create-brainflow-input-params)
          tcp-code (.get_code IpProtocolTypes/TCP)]
      (params/set-ip-protocol! p tcp-code)
      (is (= tcp-code (params/get-ip-protocol p))))))

(deftest other-info-test
  (testing "Setting and getting other info"
    (let [p (params/create-brainflow-input-params)
          test-info "test-info"]
      (params/set-other-info! p test-info)
      (is (= test-info (params/get-other-info p))))))

(deftest timeout-test
  (testing "Setting and getting timeout"
    (let [p (params/create-brainflow-input-params)
          test-timeout 5000]
      (params/set-timeout! p test-timeout)
      (is (= test-timeout (params/get-timeout p))))))

(deftest serial-number-test
  (testing "Setting and getting serial number"
    (let [p (params/create-brainflow-input-params)
          test-sn "SN12345"]
      (params/set-serial-number! p test-sn)
      (is (= test-sn (params/get-serial-number p))))))

(deftest file-test
  (testing "Setting and getting file"
    (let [p (params/create-brainflow-input-params)
          test-file "test-file.txt"]
      (params/set-file! p test-file)
      (is (= test-file (params/get-file p)))))

  (testing "Setting and getting auxiliary file"
    (let [p (params/create-brainflow-input-params)
          test-file "test-file-aux.txt"]
      (params/set-file-aux! p test-file)
      (is (= test-file (params/get-file-aux p)))))

  (testing "Setting and getting anchor file"
    (let [p (params/create-brainflow-input-params)
          test-file "test-file-anc.txt"]
      (params/set-file-anc! p test-file)
      (is (= test-file (params/get-file-anc p))))))

(deftest master-board-test
  (testing "Setting and getting master board using keyword"
    (let [p (params/create-brainflow-input-params)]
      (params/set-master-board! p :no-board)
      (is (= (.get_code BoardIds/NO_BOARD) (params/get-master-board p)))))

  (testing "Setting and getting master board using integer code"
    (let [p (params/create-brainflow-input-params)
          board-code (.get_code BoardIds/NO_BOARD)]
      (params/set-master-board! p board-code)
      (is (= board-code (params/get-master-board p))))))

(deftest to-json-test
  (testing "Converting parameters to JSON"
    (let [p (create-test-params)
          json (params/to-json p)]
      (is (string? json))
      (is (re-find #"192.168.1.100" json))
      (is (re-find #"192.168.1.101" json))
      (is (re-find #"192.168.1.102" json))
      (is (re-find #"AA:BB:CC:DD:EE:FF" json))
      (is (re-find #"/dev/ttyUSB0" json))
      (is (re-find #"7789" json))
      (is (re-find #"7790" json))
      (is (re-find #"7791" json))
      (is (re-find #"test-info" json))
      (is (re-find #"5000" json))
      (is (re-find #"SN12345" json))
      (is (re-find #"test-file.txt" json))
      (is (re-find #"test-file-aux.txt" json))
      (is (re-find #"test-file-anc.txt" json)))))

(deftest function-chaining-test
  (testing "Chaining setter functions"
    (let [p (-> (params/create-brainflow-input-params)
                (params/set-ip-address! "192.168.1.100")
                (params/set-ip-port! 7789))]
      (is (= "192.168.1.100" (params/get-ip-address p)))
      (is (= 7789 (params/get-ip-port p))))))

(run-tests)