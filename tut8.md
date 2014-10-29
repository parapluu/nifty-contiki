---
layout: page
permalink: /tutorial8/
title:  "Tutorial 4: Cooja And Nifty"
date:   2014-04-04 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

[**Files For This Tutorial**]({{ site.url }}/files/tut8.tar.gz)

# Tutorial
This tutorial will guide you through the basic ussage of Nifty in combination with Cooja. We assume, that we have a C library on our nodes that we want to interface with. The header file `answer.h` looks as following:

{% highlight C++ %}
extern int answer(int n);
{% endhighlight %}

The implementation in `answer.c` looks like this:

{% highlight C++ %}
int
answer(int n) {
  return 42 + n;
}
{% endhighlight %}

## Creating an Interface
We start creating an interface using Nifty:

{% highlight erlang %}
1> nifty:compile("answer.h", mote_answer, []).
ok
{% endhighlight %}

This will create the files `mote_answer.erl` and `contiki_app.c`. `mote_answer.erl` contains the supprt library that is required to operate the interface. `contiki_app.c` contains a Contiki application that operates the interface in the Contiki firmware.

## Compiling the firmware
When we compile the firmware, it is important to include the created `contiki_app.c` and the library sources `answer.c`. A Makefile coulde look like this:

{% highlight Makefile %}
all: contiki_app
PROJECT_SOURCEFILES+=answer.c
include $(CONTIKI)/Makefile.include
{% endhighlight %}

## Creating a Simulation
We need to create a simulation containing nodes with the firmware using the interface. It is also neccesary to use the `Socket Controll` plugin that allows us to control Cooja with Nifty.

## Starting Everything
Now we can start the simulation and call the functions. We need to subscribe to the motes serial line and wait for it to be initialized. Nifty provides the function `wait_for_msg/4` which waits for a messages on a given mote. The third argument is a timeout and the function will return `false` if it runs out. The message can be any regular expression. If it is possible to match the message against the mote output, the function returns `true`:

{% highlight erlang %}
2> Handler = nifty_cooja:start("$CONTIKI/tools/cooja", "$PWD/simulation.csc").
{handler, <7212.2.0>}
3> Motes = nifty_cooja:motes(Handler).
[1]
4> nifty_cooja:mote_listen(Handler, 1).
ok
5> nifty_cooja:wait_for_msg(Handler, 1, 1000, "Starting 'Process mote_answer'\n").
true
{% endhighlight %}

Now we can call the interface functions. In addition to the function arguments, we need to give the simulation handler and the mote ID as the firt two arguments:

{% highlight erlang %}
6> c(answer).
{ok, answer}
7> answer:answer(Handler, 1, 10).
52
{% endhighlight %}

# Debug Output
Messages starting with `DEBUG` are ignored by nifty-contiki. If you need to print some debug information, you can prefix the output with `DEBUG`:

{% highlight C++ %}
int value=42;
printf("DEBUG: important value %d\n", value);
{% endhighlight %}

# Events
Sometimes you need to return a value from an asynchronous event (in a callback function or similar). Nifty provides an event system to do this. Messages prefixed with `EVENT:` are interpreted as events and ignored during normal operations (similar to debug output):

{% highlight C++ %}
printf("EVENT:connected\n");
{% endhighlight %}

Events can be read using the `next_event/2` function:

{% highlight erlang %}
1> nifty_cooja:next_event(Handler, 2).
"connected"
{% endhighlight %}

`next_event/2` will return `fail` if no event has occured yet.

| [Previous Tutorial](tutorial7) | [Tutorial Files](files/tut8.tar.gz) | [Next Tutorial](tutorial9) |
|--------------------------------|-------------------------------------|----------------------------|
