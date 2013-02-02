---
title: Documentation
layout: documentation
---

<a id="documentation"></a>
Documentation
=============
Wookie's documentation is split into several parts:

- [Listeners](/wookie/listeners)<br>
  Listeners are how you start/stop a Wookie server
- [Routes](/wookie/routes)<br>
  Routes are how you tell Wookie how you want to handle certain requests
- [Plugins/Hooke](/wookie/plugins)<br>
  Plugins are a way to extend Wookie's functionality

<a id="quick-start"></a>
Quick start
-----------
Here are some quick examples to get you started using Wookie. Remember that
Wookie runs on top of a [cl-async](/cl-async) event loop, so you must start
Wookie from within an event loop for it to work.

### Starting Wookie
Here's how to use a [listener](/wookie/listeners#listener) to start Wookie
using the address/port you want to bind to:

{% highlight cl %}
;; create a listener that accepts connections on port 80
(let ((listener (make-instance 'listener
                               :bind nil  ; equivalent to "0.0.0.0"
                               :port 80)))
  ;; start an event loop and pass our listener to Wookie's start-server method
  (as:start-event-loop
    (lambda () (start-server listener))
    ;; it's generally a good idea to catch errors here (otherwise they bubble
    ;; up to the REPL and Wookie won't have a chance to handle them)
    :catch-app-errors t))
{% endhighlight %}
