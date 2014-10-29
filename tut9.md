---
layout: page
permalink: /tutorial9/
title:  "Tutorial 5: Testing With Proper"
date:   2014-10-29 11:55:46
tags: [tutorial]
image:
  feature: nifty_trouble.png
---

[**Files For This Tutorial**](../files/tut9.tar.gz)

# Tutorial
This tutorial will guide you through using Nifty Contiki together with PropEr, a property based testing framework. This tutorial covers the specifics using Nifty Contiki. It is strongly recomended to read the [Proper Documentation](http://proper.softlab.ntua.gr/).

## Firmware
We use the same Firmware as in the [former Tutorial](../tutorial8).

## Testcase
We need to start up the simulator with the right simulation file, subscribe to the motes and wait until the nodes have been initialized:

{% highlight erlang %}
setup() ->
    {ok, Handler} = nifty_cooja:start("$CONTIKI/tools/cooja", "answer_sim.csc", []),
    Motes = nifty_cooja:motes(Hanlder),
    [ok = nifty_cooja:mote_listen(Handler, MoteID) || MoteID <- Motes],
    [true = nifty_cooja:wait_for_msg(Handler, MoteID, 1000, "Starting 'Process answer'\n") || MoteID <- Motes].	
{% endhighlight %}

Now we can write our property:
{% highlight erlang %}
answer_property() ->
	?FORALL(I, integer(), mote_answer:answer(Handler, Mote, I) =:= I+10).
{% endhighlight %}

In order for the code to work, we need to get the Handler of the simulation:
{% highlight erlang %}
answer_property() ->
    ?FORALL(I, integer(),
        begin
            {running, Handler} = nifty_cooja:state(),
            mote_answer:answer(Handler, Mote, I) =:= I+10
        end).
{% endhighlight %}

Now we just need to take care about the `Mote` argument. We want the motes to be generated, like `I`. One way of doing this is by parameterizing our property with a list of motes:

{% highlight erlang %}
answer_property(Motes) ->
    ?FORALL({I, Mote}, {integer(), oneof(Motes)},
            begin
		        {running, Handler} = nifty_cooja:state(),
                mote_answer:answer(Handler, Mote, I) =:= I+42
            end).
{% endhighlight %}

Now we can run our tests:

{% highlight erlang %}
1> c(tut).
{ok, tut}
2> tut:setup().
[true, true]
3> {running, Handler} = nifty_cooja:state().
{running, <0.33.0>}
4> Motes = nifty_cooja:motes(Handler).
[1,2]
5> proper:quickcheck(tut:answer_property(Motes)).
...................................................................
ok
6> nifty_cooja:exit().
ok
{% endhighlight %}

Now we want to automate this setup and finalizing phase. We can do this by packing the code into an EUnit testcase:

{% highlight erlang %}
answer_test_() ->
    {timeout, 3600, ?_assertEqual(true, test_func())}.

test_func() ->
    setup(),
	{running, Handler} = nifty_cooja:state(),
	Motes = nifty_cooja:motes(Handler),
    R = proper:quickcheck(answer_property(Motes), [{to_file, user}]),
    nifty_cooja:exit(),
    R.
{% endhighlight %}

## Self-Contained Tests
Our testcase is not self-contained yet. For each generated input we progress the simulation a bit further. This means, that the flowing tests depend on the state the simulation was left in from former tests. This is bad, because we can not be sure to reproduce an error if we find one.

We can fix this by pulling the simulation setup into the property. We still need to give the mote IDs to the property:
{% highlight erlang %}
setup() ->
    Handler = nifty_cooja:start("$CONTIKI/tools/cooja", "$PWD/simulation.csc", [gui]),
    Motes = nifty_cooja:motes(Handler),
    [ok = nifty_cooja:mote_listen(Handler, MoteID) || MoteID <- Motes],
    [true = nifty_cooja:wait_for_msg(Handler, MoteID, 1000, "Starting 'Process mote_answer'\n") || MoteID <- Motes],
    Handler.

answer_property(Motes) ->
    ?FORALL({I, Mote}, {integer(), oneof(Motes)},
            begin
        		Handler = setup(),
		        R = mote_answer:answer(Handler, Mote, I) =:= I+42,
        		nifty_cooja:exit(),
        		timer:sleep(100),
        		R
            end).

test_func() ->
    Handler = setup(),
    Motes = nifty_cooja:motes(Handler),
    nifty_cooja:exit(),
    proper:quickcheck(answer_property(Motes), [{to_file, user}]).

answer_test_() ->
    {timeout, 3600, ?_assertEqual(true, test_func())}.
{% endhighlight %}

We can now run the test:

{% highlight erlang %}
6> c(tut).
{ok, tut}
7> nifty:compile("answer.h", mote_answer, []).
generating...
ok
8> c(mote_answer).
{ok, mote_answer)
9> eunit:test(tut).
..................................................
..................................................
OK: Passed 100 test(s).
  Test passed.
ok
{% endhighlight %}

| [Previous Tutorial](../tutorial8) | [Tutorial Files](../files/tut9.tar.gz) | [Next Tutorial](../tutorial_trouble) |
|-----------------------------------|----------------------------------------|--------------------------------------|
