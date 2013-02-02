---
title: Routes | Documentation
layout: documentation
---

Routes
======
This section explains how to get Wookie to run different sections of your code
depending on what's being requested.

The Wookie routing system uses regular expression routing (the default) or exact
match string routing. Wookie routes on the HTTP method of the request (`GET`,
`POST`, `PUT`, `DELETE`, etc) and the resource being requested (ie the URL).

Routes are added to the routing table via [defroute](#defroute) (and are added
to the end of the table). When searching for a matching route when a request
comes in, routes at the beginning of the routing table are given preference.

Routes are capable of handling streaming (chunked) data. Routes can also tell
the router that they are the wrong route for the job and that the routing
system should use the next matching route (known as "route jumping," done via
the [next-route](#next-route) function).

- [defroute](#defroute) _macro_
- [clear-route](#clear-route) _function_
- [clear-routes](#clear-routes) _function_
- [next-route](#next-route) _function_
- [route-error](#route-error) _condition_
- [route-not-found](#route-not-found) _condition_

<a id="defroute"></a>
### defroute (macro)
{% highlight cl %}
(defmacro defroute ((method resource &key (regex t) (case-sensitive t)
                                          chunk replace)
                    (bind-request bind-response &optional bind-args)
                    &body body))
{% endhighlight %}

This sets up a route in the routing system such that when a request comes in
that matches on the method/resource, the body will be run.

`:regex` specifies whether the `resource` is a regular expression (matched with
[cl-ppcre](http://weitz.de/cl-ppcre/)). If a regular expression is selected
(the default) then regular expression groups can be accessed from the route via
the `bind-args` argument. `:case-sensitive` specifies if the route's regex is
case-sensitive.

`:chunk` tells us that this route is more than willing to stream chunked
content. In other words, we'll set up a handler in our route using [with-chunking](/wookie/request-handling#with-chunking)
to stream content over HTTP.

`:replace` tells the routing system that this route should replace the first 
route with the same method/resource in the routing table.

`bind-request` and `bind-response` are the variables we want to be available to
the route body that hold our respective `request` and `response` objects for the
incoming request. If `bind-args` is passed, it will be a list that holds all the
matched groups from the `resource` regex.

Let's dive in with a few examples:

{% highlight cl %}
;; Set up a route that serves up our app's homepage
(defroute (:get "/") (req res)
  (send-response res :body "Welcome to my app!"))

;; Grab an album by its numeric ID
(defroute (:get "/albums/([0-9]+)") (req res args)
  ;; our album id is in `args`
  (let ((album (get-album-by-id (car args))))
    (if album
        (progn
          ;; set the Content-Type for the response
          (setf (getf (response-headers res) :content-type) "application/vnd.myapp.album+json")
          ;; send back JSON for the album we found
          (send-response res :body (yason:encode (my-app:get-album-by-id album-id))))
        ;; NOPE
        (send-response res :status 404 :body "That album wasn't found =["))))

;; set up a route that can handle chunking
(defroute (:post "/files" :chunk t) (req res)
  (with-chunking req (chunk-bytes finishedp)
    (my-app:send-content-chunk-to-storage chunk-bytes)
    (when finishedp
      (my-app:finish-file)
      (send-response res :body "Thanks for the file."))))
{% endhighlight %}

<a id="clear-route"></a>
### clear-route (function)
{% highlight cl %}
(defun clear-route (method resource-str))
  => nil
{% endhighlight %}

Removes all routes that match the given method/resource from the routing table.

{% highlight cl %}
;; add a route
(defroute (:get "/friends") (req res) ...)

;; clear out the route we just added
(clear-route :get "/friends")
{% endhighlight %}

<a id="clear-routes"></a>
### clear-routes (function)
{% highlight cl %}
(defun clear-routes ())
  => nil
{% endhighlight %}

Clear out all routes in the routing table.

<a id="next-route"></a>
### next-route (function)
{% highlight cl %}
(defun next-route ())
  => nil
{% endhighlight %}

This function allows a route to notify the routing system that it's not the
right route (even though it matched on method/resource). There are a few
reasons this could be useful (for instance if you want to route based on a
specific `Accept` header).

{% highlight cl %}
;; define a route that returns a file
(defroute (:get "/thefile") (req res)
  (if (probe-file "thefile")
      ;; found the file...serve it!
      (send-response :body (get-file-contents "thefile"))
      ;; file not found, let another route try
      (next-route)))

;; define a catch-all route
(defroute (:get ".+") (req res)
  (send-response res :status 404 :body "What you're looking for isn't here."))
{% endhighlight %}

<a id="route-error"></a>
### route-error (condition)
A condition that describes a general error with the routing system.

<a id="route-note-found"></a>
### route-not-found (condition)
_extends [route-error](#route-error)_

This is a condition that's thrown when a route for the current method/resource
is not found.

