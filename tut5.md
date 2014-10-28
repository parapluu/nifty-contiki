---
layout: page
permalink: /tutorial5/
title:  "Tutorial 5: Starting Cooja"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

# Starting Cooja
Nifty provides the `nifty_cooja` module to give you access to Cooja's functionality. You can start the simulator with the following command:

{% highlight erlang %}
1> nifty_cooja:start("$CONTIKI/tools/cooja",
                     "path/to/simfile.csc", []).
{% endhighlight %}

The first parameter should point to the directory containing Cooja. This is typically in the `tools/cooja` directory of your Contiki distribution. The second parameter should point to the simulation you want to start Cooja with. In order to be able to control Cooja from Erlang, make sure to have the `Socket Control` plugin enabled in the used simulation. The return value is either `{handler, Handler}` in case of success, or fail if something went wrong.  `Handler` is used to communicate with Cooja and is used for almost all functions in `nifty_cooja`

The third parameter is a list of options. The currently supported opions are `debug` and `gui`. `debug` will print additional debugging information, like the Cooja command line output. `gui` will run Cooja with the gui allowing you to observe and manipulate the simulation by hand. 

# State
You can get the state of Cooja by running:

{% highlight erlang %}
2> nifty_cooja:state().
{% endhighlight %}

which will return `{running, Handler}`, `not_running`, `ok`, or `fail`. `ok` and `fail` is returned once! after Cooja has been exited (or crashed), while the other two indicate a running or not running simulator. `{running, Handler}` also gives you the handler to the currently running Cooja simulation.

# Shuting Cooja Down
The currently running Cooja simulation can be shut down with:

{% highlight erlang %}
3> nifty_cooja:exit().
{% endhighlight %}

This will return `not_running`, `ok`, or `fail`, depending on the state of Cooja and the return value.


| <a  href="{{ site.url }}/tutorial6">Next Tutorial</a> |
|-------------------------------------------------------|
