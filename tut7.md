---
layout: page
permalink: /tutorial7/
title:  "Tutorial 3: Controlling The Motes"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---
# Motes

To get list of all motes that are currently in the simulation, you can run:

{% highlight erlang %}
1> nifty_cooja:motes(H).
[1,2,3,4]
{% endhighlight %}

which will return a list of mote IDs. You can use this mote IDs to get more information about the motes, like the current position, or to perform mote specific actions:

{% highlight erlang %}
2> nifty_cooja:mote_get_pos(H, 1).
{10.967432483340511,42.75126729507585,0.0}
3> nifty_cooja:mote_set_pos(H, 1, {20.0,10.0,0.0}).
ok
{% endhighlight %}

# Add And Delete Motes
To delete motes from the simulation, you just need to invoke the following command:
{% highlight erlang %}
4> nifty_cooja:mote_del(H, 2).
ok
5> nifty_cooja:motes(H).
[1,3,4]
{% endhighlight %}

If you want to add a mote, you need to specify a mote type. Mote types are predefined in the simulation file. You can get a list of all the defined mote types:

{% highlight erlang %}
6> nifty_cooja:mote_types(H).
["sky1","sky2"]
{% endhighlight %}

To add a new mote use the `mote_add/2` function. The new mote will always be placed at postion `{0.0, 0.0, 0.0}` so it is a good idea to move it to a reasonable place after you added it.

{% highlight erlang %}
6> nifty_cooja:mote_add(H, "sky2").
5
7> nifty_cooja:mote_set_pos(H, 5, {10.0, 20.0, 0.0}).
ok
{% endhighlight %}

# Serial IO
To use the radio communication interface, you need to subscribe to it:
{% highlight erlang %}
8> nifty_cooja:mote_listen(H, 5).
ok
{% endhighlight %}

This means, that from now on, we can write and read messages from the serial line:

{% highlight erlang %}
8> nifty_cooja:mote_write(H, 5, "echo Hello").
ok
9> nifty_cooja:simulation_step(H, 1000).
ok
10> nifty_cooja:mote_read(H, 5).
"Hello\n"
{% endhighlight %}

To unsubscribe, we can call `mote_unlisten/2`.


# Hardware Events
Similar to the serial IO, we need to subscribe to the hardware events if we want to record them.

{% highlight erlang %}
11> nifty_cooja:mote_hw_listen(H, 2).
ok
12> nifty_cooja:simulation_step(H, 1000).
ok
13> nifty_cooja:mote_hw_events(H, 2).
[{radio,["on","10773296"]},
 {radio,["off","10773745"]},
 {radio,["on","10774480"]},
 {radio,["off","10774929"]},
 {radio,["on",[...]]},
 {radio,[[...]|...]},
 {radio,[...]},
 {radio,...},
 {...}|...]
{% endhighlight %}


| [Previous Tutorial](../tutorial6) | [Next Tutorial](../tutorial8) |
|-----------------------------------|-------------------------------|
