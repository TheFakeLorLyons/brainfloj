# BrainFloj

**BrainFloj** is a Clojure wrapper for the [BrainFlow](https://brainflow.org) API for real-time EEG data streaming, analysis, and interaction. It connects directly to BrainFlow’s Java API, giving you full access to biosignal data from supported devices — with the expressive power of functional, data-driven Clojure code.

## Project Structure
The codebase is organized for clarity and modularity:
   - All primary source code lives in src/floj, including the core logic for interfacing with BrainFlow.
   - Tests are located in src/tests, and they’re run using the Cognitect test runner for a straightforward and idiomatic Clojure testing workflow.
   - The examples/ directory contains runnable demos built using BrainFloj — including a prototype of a brain-controlled Pong game, with more examples on the way.
   - Native BrainFlow dependencies __need to be downloaded__ via the `setup.clj` file located directly in `src/`. Running `clj -m setup` at the command line, or evaluating `(setup-brainfloj!)` via the repl.
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
   - Run the CLI in the root directory with the command `clojure -M:flow -m floj.cli`
      - It will prompt you to connect a device. If you do not connect a device, it will begin with a default 'SYNTHETIC_BOARD'
         - It is possible to take recordings with the synthetic board and read them with the build in file reader in the CLI, but the numbers aren't usable for anything meaningful at this time (except maybe some random number generation).

## Running Tests
   - Currently the instructions below don't work and you need to individually run tests in each namespace to test them. 
      - Enter the REPL and evaluate `(run-tests)` - I will fix the test-runner in a coming update.
   - ~~Tests are written in the src/tests directory and use the Cognitect test runner.~~
   - ~~Run them with:~~
       ~~'clj -X:test'~~

## Example: Brain-Controlled Pong & EEG Visualizer
   - An EEG visualizer (currently meant for the openBCI ganglion specifically) exists in floj.visualizer. It displays a live and calibrated incoming eeg signal for each signal, indicating the average frequency (of the dominant band between alpha nad beta), and magnitude of alpha and beta waves.

   - You can try the first example (a __brain-controlled Pong prototype__) in the examples/ directory. It demonstrates how to hook into the brainfloj.core interface and respond to live EEG input.
      - First, navigate to the examples/Pong directory
      - Then enter the command:
              `clj -A:dev -X dev/-main`
              - Pong should open up in the browser at http://localhost:8080

   - More demos and experiments coming soon...

## Roadmap
   - Better signal calibration and preprocessing tools
   - Pluggable control setups for games, music, and art
   - Full API coverage of BrainFlow features (including filtering, feature extraction, etc.)
   - Live dashboard using Electric Clojure
   - Friendly REPL-centric dev UX


## 🙏 Thanks

This project exists because of the support, mentorship, and encouragement of a number of amazing humans.

### Mentorship
- **Dustin Getz**: Dustin has constantly encouraged me to succeed as quickly as possible, and challenges me to push my boundaries. His support serves as the foundation for making this project a reality, as I would not have ever sought funding without his support, guidance, and mentorship.
- **Dav Clark**: I met Dav at the Clojure Conj, and I enthusiastically brought up my interest in BCIs to him and found that we are both very interested in this technology, and he has over a decade of experience in neurotechnology working with MEG systems. He has been a support and guide in the huge world of BCI.
- **Daniel Slutsky**: Daniel first proposed that I should submit a talk to reClojure, which serves as the impetus for the fundraising for this project. Daniel has also gotten me involved in the scientific Clojure community; and serves as an inspiration and support for me across my varied projects.
- **Cameron Barre**: Cameron has been helping me to improve my coding and design skills over the last year or so, and has served as a mentor and donator to this project. He has been teaching me about event modeling and system design, and has shown me a lot about the higher order aspects of programming.
- **Jason Bullers**: Jason was one of the first people I met in the Clojure community who took on a role of mentor to me. It is safe to say that I would probably be half the programmer I am today without his help and encouragement.
- **SenLong Yu**: SenLong has been helping me in all aspects of my coding since I first decided to pursue programming as a career. We have worked on several projects together, and his knowledge and supprt have made this possible; as I would not have the confidence to tackle this project if it was not for him.

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

---



__Clojure wrapper for brainflow (MIT License)__

__Original API by Andrey Parfenov, 2018 (many thanks!)__