# BrainFloj

__A Clojure wrapper around openBCI's BrainFlow API__

**BrainFloj** is a Clojure wrapper for the [BrainFlow](https://brainflow.org) API for real-time EEG data streaming, analysis, and interaction. It connects directly to BrainFlow’s Java API, giving you full access to biosignal data from supported devices — with the expressive power of functional, data-driven Clojure code.

Further details coming soon

- In the root directory:
`clj -M:flow -m floj.cli`    starts the CLI
`clj -M:flow -m app.server`  starts the pong server
`npx shadow-cljs watch pong` starts the pong client
   - Don't forget to npm install first!
   - Vistit http://localhost:8080/example/index.html to see the pong game