![Alpha](https://img.shields.io/badge/status-alpha-orange)
![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)
![Clojure CLI](https://img.shields.io/badge/built%20with-Clojure%20CLI-5881d8)

# BrainFloj (Alpha)

**BrainFloj** is a Clojure wrapper and extension for the [BrainFlow](https://brainflow.org) API for real-time EEG data streaming, analysis, and interaction. It connects directly to BrainFlow’s Java API, giving you full access to biosignal data from supported devices — with the expressive power of functional, data-driven Clojure code.

## 🔬 How It Works
BrainFloj revolves around a persistent .lor/ directory in the user's home folder. This is where all data, profiles, logs, and configuration live. It enables structured, reproducible, and extensible EEG data workflows for real-time brain-computer interaction.

__Future Vision__: One of the ultimate goals (other than a general purpose brain-app development library) is "brain-macs" — a customizable brain-wave keyboard shortcut application (think C-s but fostering specific brain macros instead of keystrokes). The foundational classification system is **already complete**; custom brain macros are just a CLI feature away!

## 🚀 Quick Start
1. Clone the repo
2. Install BrainFlow native deps:
   ```bash
   clj -M -m setup
   ```
3. Run the CLI
   ```bash
   clojure -M:flow -m floj.cli
   ```
   - It will prompt you to set up a profile, and in doing so prompt you to set up a device.
   - You can run brainflow with a SYNTHETIC_BOARD without a physical BCI, but the visualizer will not work without a real board
     - The Pong game works without a BCI

## Project Structure
The codebase is modular and structured for clarity and extensibility:
   - The actual BrainFlow wrapper exists in src/floj/brainflow - this code alone is enough to make robust BCI applications! The code existing around this is meant to be a general purpose library to make that task simpler for devs who want to start using recordings in an applied setting right away!
   - All primary source code lives in src/floj, including the core logic for interfacing with BrainFlow.
   - Tests are located in src/tests, ~~and they’re run using the Cognitect test runner for a straightforward and idiomatic Clojure testing workflow.~~
     - The test runner has yet to be implemented, but existing tests can be run with `(run-tests)` at the bottom of the test namespaces (or in the REPL).
     - I have yet to write tests for all modules, but plan to include more extensive testing for all modules in the future. 
   - The examples/ directory contains runnable demos built using BrainFloj — including a prototype of a brain-controlled Pong game, with more examples on the way.
   - **IMPORTANT** Native BrainFlow dependencies __need to be downloaded__ via the `setup.clj` file located directly in `src/`. Running `clj -m setup` at the command line, or evaluating `(setup-brainfloj!)` via the repl.
      - Note that it is required to run `(clean-deps)` in order to remove any local paths that remain in your deps.edn file prior to pushing any derivative works in order to ensure that there are no conflicting classpaths leftover.
   - Configuration is managed via deps.edn, following standard Clojure CLI conventions.

## Floj CLI
   - The CLI exists to allow simple and seamless connection to any openBCI (or supported) eeg device (see floj/brainflow/board_types.clj)
   - It allows users to:
      - Connect to their device (currently directly via mac address and COM port, soon to support bluetooth)
      - Calibrate their device for optimal individualized recordings
      - Save recordings for later use/analysis/simulation
      - Record waveforms and create individualized 'categories' of 'wave-signatures' for the creation of various BCI applications.
      - (Soon) Tag files with important moments to enhance calibration and demarcate important moments.
      - (Soon) Custom keystokes to enable general purpose brain computing input at the command line.
   - Run the CLI in the root directory with the command `clj -M:flow -m floj.cli`
      - It will prompt you to connect a device. If you do not connect a device, it will begin with a default 'SYNTHETIC_BOARD'
         - It is possible to take recordings with the synthetic board and read them with the build in file reader in the CLI, but the numbers aren't usable for anything meaningful at this time (except maybe some random number generation).

## .lor/ Directory Overview

   ```
   ~/.lor/
   ├── logs/                   # Development/debugging logs
   │   ├── io_logs/            # Input/output operation logs
   │   ├── device_logs/        # BCI device communication logs
   │   └── app_logs/           # Application-level logs
   ├── profiles/               # User profile directories
   │   └── [profile-name]/
   │       ├── history/        # Profile version history
   │       └── wave-lexicon/   # Wave signature categories
   ├── command-map.edn         # CLI keybinding configuration
   └── config.edn              # Global application state
   ```

### `logs/`
   #### Contains:

   - io_logs/, device_logs/, app_logs/

   - These are mostly for debugging or advanced introspection. Not required for standard usage, but available for developers who want to integrate BrainFloj into more complex applications.

### `command-map.edn`
   #### Defines CLI key bindings. 
   - Future versions will allow users to map brainwave patterns to custom macros — the vision is to create brain-driven computing, like Emacs but powered by EEG. For example, C-s could be triggered by a specific brain signal, not a keypress.
      - This functionality is technically fully possible now that the classification infrastructure is complete. It's just waiting to be implemented — help welcome!

### `config.edn`
   #### Tracks global state for BrainFloj: 
   - In this case - just the currently active profile. This is how the system knows which profile to load and how many recordings the user has taken.
     - ex: `{:active-profile "new-profile" :recording-counter 123}`

### `.lor/profiles/`
   - Each user profile is a directory under .lor/profiles/, and contains:
      - __history/__
         - Stores a series of historical profile.edn snapshots. Every 10 recordings, a new snapshot is saved. Once 10 snapshots exist, they are consolidated to recalibrate the profile. This enables ongoing personalization without bloating disk usage.
         - `:name, :created-at, :updated-at, :bci-device....` 
         - example:
         - `{:bci-device {:device-type "GANGLION_BOARD", :board-id 1, :mac-address "a1:b2:c3:d4:e5:f6", :com-port "COM3"}}`
            - __:golden-tensor__
               - The golden tensor is a personalized EEG baseline used to normalize and calibrate future readings.
               - The golden tensor is rather complex and will likely get its own documentation in the future. It serves as a reference point for the other calibration files
      
      - __wave-lexicon/__
         - This is the heart of brain-controlled interaction. It contains one or more categories (e.g., pong/) which in turn contain:
            - Wave-signatures (e.g., up/, down/)
              - Each signature is a directory of recordings
                - Recordings are composed of:
                  - .lor files (one per EEG channel)
                  - recording_metadata.edn - aggregate information about the current recording.
                  - signature_features.edn - triangulates new data with prior patterns (only in the case of wave-signature recordings)
                  - tags.edn - demarcated spots in the recording. Unimplemented right now, but will be useful to designate when actions occur during recordings in the future.
               -  Each wave-signature has a signature.edn that aggregates feature data across all its recordings. The more recordings, the more accurate the signal detection becomes.
         - At the top level of a category, there’s a category.edn, which aggregates all signature.edn files. This allows for fast, memory-efficient signal matching without recomputing everything.
         - Every level — recording, signature, and category — is compared against the user's golden tensor for improved personalization and signal alignment.
         - Developers can treat wave-signatures somewhat like enums. Just record, categorize, and match — no deep neuroscience or math required.
         ```
         wave-lexicon/
         └──[category]/            # e.g., "pong", "music-control", "meditation"
            ├── category.edn        # Aggregated data for entire category
            ├── [signature-name]/   # e.g., "up", "down", "focus", "relax"
            │   ├── signature.edn   # Aggregated data across all recordings
            │   └── [recording]/    # Individual recording sessions
            │       ├── 1.lor       # Channel 1 data
            │       ├── 2.lor       # Channel 2 data
            │       ├── n.lor       # Channel n data
            │       ├── recording_metadata.edn
            │       └── signature_features.edn  # Triangulated features
            └── ...
         ```
## 🚧 Running Tests (In Progress)
   - Currently the instructions below don't work and you need to individually run tests in each namespace to test them. 
      - Enter the REPL and evaluate `(run-tests)` - I will fix the test-runner in a coming update.
   - ~~Tests are written in the src/tests directory and use the Cognitect test runner.~~
   - ~~Run them with:~~
       ~~'clj -X:test'~~

## Examples: Brain-Controlled Pong & EEG Visualizer
   - An EEG visualizer (currently meant for the openBCI ganglion specifically) exists in floj.visualizer. It displays a live and calibrated incoming eeg signal for each signal, indicating the average frequency (of the dominant band between alpha nad beta), and magnitude of alpha and beta waves.

   - You can try the first example (a __brain-controlled Pong prototype__) in the examples/ directory. It demonstrates how to hook into the brainfloj.core interface and respond to live EEG input.
      - First, navigate to the examples/Pong directory
      - Then enter the command:
              `clj -A:dev -X dev/-main`
              - Pong should open up in the browser at http://localhost:8080
   
   - The EEG Visualizer exists under src/floj/visualizer/core.clj
     - You need to call main with your active profile name as a string arg, and it will connect the device and collect 256 samples, and then begin a live recording that visualizes the current average frequencies and magnitudes across all channels.

   - More demos and experiments coming soon...

## ✅ What Works Today (Alpha Features)
- Real-time EEG streaming from OpenBCI devices
  - brainfloj has simple reading of recording directories in addition to writing. 
  - In the CLI press 'R' to see the list of previous recordings, and you can choose them by number (1 being most recent, n being oldest).
  - Eventually I want to allow functionality to export these recordings as matlab files (see below).
- Wave-signature recording and categorization.
- Golden tensor calibration system.
- Brain-controlled Pong demo.
- EEG visualizer with frequency analysis.

## Roadmap
### 🔥 High Priority (Next several releases)
   - I will be including architecture diagrams in resources/diagrams coming soon.
   - A UI calibration module is going to be one of the next projects, to make recording easier for non-devs.
   - Bluetooth connection so that manual connection is not required, although manual connection is actually fairly simple already.
     - brainfloj already automatically prompts users to connect to their device once one has been configured via the CLI.
### 🚀 Medium Priority (Within months, by the end of the year...)
   - While brainfloj is meant to work with all BrainFlow compatible devices, I am pretty sure that I hardcoded 4 channels in multiple locations since I am using a GANGLION_BOARD. I'll fix this soon, but technically it should still work with devices that have more than 4 channels, using just the first four channels of those devices for the time being. At least for regular recordings and wave-signature recordings it should work for all devices though, so that is most functionality.
   - A digital twin of the golden-tensor to exist in memory during live recordings.
   - Full API coverage of BrainFlow features (including filtering, feature extraction, etc.).
   - Better signal calibration and preprocessing tools.
   - Pluggable control setups for games, music, and art.
### 🧪 Long Term (When possible)
   - Incorporate machine learning libraries in order to increase the accuracy of classifications.
   - Friendly REPL-centric dev UX.
   - .lor to .mat conversions for future data analysis in matlab.

### 🤝 Inspired??? I’d love your help!
     👉 PRs, issues, and questions are all welcome. Let's see how we can collaborate!!

## 🙏 Thanks
This project exists because of the support, mentorship, and encouragement of a number of amazing humans.

### Mentorship
- **Dustin Getz**: Dustin has constantly encouraged me to succeed as quickly as possible, and challenges me to push my boundaries. His support serves as the foundation for making this project a reality, as I would not have ever sought funding without his support, guidance, and mentorship.
- **Dav Clark**: I met Dav at the Clojure Conj, and I enthusiastically brought up my interest in BCIs to him and found that we are both very interested in this technology. He has over a decade of experience in neurotechnology working with MEG systems, and has been a support and guide in the huge world of BCI.
- **Daniel Slutsky**: Daniel first proposed that I should submit a talk to reClojure, which served as the impetus for the crowdfunding of this project. Daniel has also gotten me involved in the scientific Clojure community ([SciNoj](scicloj.github.io/)); and serves as an inspiration and support for me across my varied projects.
- **Cameron Barre**: Cameron has been helping me to improve my coding and design skills over the last year or so, and has served as a mentor and donator to this project. He has been teaching me about event modeling and system design, and has shown me a lot about the higher order aspects of programming.
- **Jason Bullers**: Jason was one of the first people I met in the Clojure community who took on a role of mentor to me. It is safe to say that I would probably be half the programmer I am today without his help and encouragement.
- **SenLong Yu**: SenLong has been helping me in all aspects of my coding since I first decided to pursue programming as a career. We have worked on several projects together, and his knowledge and supprt have made this possible; as I would not have the confidence to tackle this project if it was not for him.
- **Olav Fosse**: Olav has given me a backboard to talk about the theory behind brainfloj and helped me a lot in understanding Electric Clojure in order to apply my library to a live Pong game. His encouragement and advice helped me actually implement what was only theory beforehand.

**And a great many other people from the Clojure Camp/SciCloj, and the Clojure community at large! Thank you all so much for helping me get to this point!**

## Sponsorship
### $300 Donors
- **ObneyAI**
- **Cameron Barre**
- **Dustin Getz**

#### Other Donors
- **Amar Mehta** - $100
- **Eugene Pakhomov** - $100
- **Mark Addleman** - $100
- **MTrost** - $100
- **Cameron Desautels** - $80
- **Justin Tirrell** - $50
- **Ian Chow** - $25
- **John Tyler** - $20
- **Brandon Ridge** - $20
- **Private Donor** - $5

### Community + Inspiration
- The [Clojure Camp](https://my.clojure.camp/)
- The [Clojurists Together](https://clojuriststogether.org)
- The [SciNoj](https://scicloj.github.io/)

Thank you for helping me bring BrainFloj into the world!!!

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Lor

---


__Clojure wrapper for brainflow (MIT License)__

__Original API by Andrey Parfenov, 2018 (many thanks!)__