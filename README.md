Piddling Plugins
----------------

Piddling Plugins is a simple way to enable plugin-style extensions to
an application which can be enabled and disabled dynamically. It
operates by using `CHANGE-CLASS` to add and remove
behavior mixins from an object.

It was originally written for the [Shuffletron][shuf] music player. I considered naming it "trivial-plugins" but decided that was too presumptuous.

  [shuf]: http://github.com/ahefner/shuffletron

Example
-------

A deliberately simplified example of how this might be used in a music player application:

    (defclass music-player () ())

    (defun run-music-player ()
      ;; You need to set or bind *application* to your application instance
      ;; if you use defun-extensible. It's a good idea even if you don't.
      (let ((*application* (make-instance 'music-player)))
        (init-audio)
        (init-library *application*)
        (loop (execute-command (read-line)))))

Functions extensible by plugins can be defined using `DEFUN-EXTENSIBLE`.
It's just syntactic sugar for defining a generic function specialized
on the application object, with a wrapper that passes in the value of `*APPLICATION*`.

    (defun-extensible execute-command (command)
      ...)

    (defun-extensible play-song (song)
      ...)

    (defun-extensible song-finished (song)
      ...)

Two pretend examples of plugins, minus the guts:

    (defclass scrobbler ()
      ((auth-token :accessor auth-token)))

    (defmethod plugin-enabled (app (plugin-name (eql 'scrobbler)) &key &allow-other-keys)
      (setf (auth-token app) (get-auth-token))
      (format t "~&Scrobbler enabled.~%"))

    (defmethod plugin-disabled (app (plugin-name (eql 'scrobbler)))
      (format t "~&Scrobbler disabled.~%"))

    (defmethod extending-song-finished :after ((plugin scrobbler) song)
      (scrobble song (auth-token plugin)))

    (defclass status-bar () ())

    (defmethod extending-play-song :after ((plugin status-bar) song)
      (redraw-status-bar))

    (defmethod extending-execute-command :after ((plugin status-bar) command-line)
      (declare (ignore command-line))
      (redraw-status-bar))

To enable a plugin:

    (enable-plugin *application* NAME [INITARGS...])

To disable a plugin:

    (disable-plugin *application* NAME)

To set precisely which plugins are enabled:

    (reconfigure *application* LIST-OF-PLUGINS [INITARGS...])

References
----------

 * Strandh R., Hamer J., Baumann G. "Using Stealth Mixins to Achieve Modularity" (2007)
  * Implentation in Gsharp: <http://common-lisp.net/cgi-bin/gitweb.cgi?p=projects/gsharp/gsharp.git;a=blob;f=utilities.lisp>
 * "Modes" implementation in McCLIM's ESA framework (formerly part of Climacs)
  * <http://git.boinkor.net/gitweb/mcclim.git/blob/HEAD:/ESA/utils.lisp#l446>
 * Throwaway classes (comp.lang.lisp): <http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/78ef4a5fcd6a1661?pli=1>
 * The Art of the Metaobject Protocol, Section 2.4
  * <http://www.foldr.org/~michaelw/lisp/amop-programmatic-class.lisp>
 * ContextL - <http://common-lisp.net/project/closer/contextl.html>
 * Dynamic Classes - <http://common-lisp.net/project/dynamic-classes/>
