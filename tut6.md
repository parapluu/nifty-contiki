---
layout: page
permalink: /tutorial6/
title:  "Tutorial 6: Controlling The Simulation"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---
# Start and Stop
To start a simulation ise the following command:

{% highlight erlang %}
1> nifty_cooja:start_simulation(Handler).
ok
{% endhighlight %}

This will run the simulation with the set speed limit. To change the speed limit, you can use the `nifty_cooja:set_speed_limit/2` function:

{% highlight erlang %}
2> nifty_cooja:set_speed_limit(Handler, 1.0).
ok
{% endhighlight %}

A value of `1.0` corresponds to a speed limit of 100%.

A simulation can be stopped with:

{% highlight erlang %}
3> nifty_cooja:start_simulation(Handler).
ok
{% endhighlight %}

It is also possible to progress the simulation by fixed time slices:

{% highlight erlang %}
4> nifty_cooja:simulation_step(Handler, 1000).
ok
5> nifty_cooja:simulation_step_ms(Handler).
ok
{% endhighlight %}


# Time
You can get the current simulation time using the following commands:
{% highlight erlang %}
6> nifty_cooja:simulation_time().
10726000
7> nifty_cooja:simulation_time_ms().
10726
{% endhighlight %}

# Radio
Nifty Contiki offers the possibility to change the radio setup during the simulation. To get the current setup you can call:

{% highlight erlang %}
8> nifty_cooja:radio_get_config(H).
{"class org.contikios.cooja.radiomediums.UDGM",
 [{"COUNTER_INTERFERED",31},
  {"COUNTER_RX",70},
  {"COUNTER_TX",101},
  {"INTERFERENCE_RANGE",100.0},
  {"SUCCESS_RATIO_RX",1.0},
  {"SUCCESS_RATIO_TX",1.0},
  {"TRANSMITTING_RANGE",50.0}]}
{% endhighlight %}

The return value contains the type of the radio and a list of options that are available. The type is fixed and cannot be changed, but the options can be altered. To change the radio setup, you need to construct a tuple `{radio_type, Options}` containing the options you want to set. You can then set the options with the following command:

{% highlight erlang %}
9> nifty_cooja:radio_set_config(H,
      {"class org.contikios.cooja.radiomediums.UDGM",
       [{"TRANSMITTING_RANGE",25.0}]}).
ok
{% endhighlight %}

Currently only the UDGM radio offers these kind of option.

| <a  href="{{ site.url }}/tutorial5">Previous Tutorial</a> |  <a  href="{{ site.url }}/tutorial7">Next Tutorial</a> |
|-----------------------------------------------------------|--------------------------------------------------------|
