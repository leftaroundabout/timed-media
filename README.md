Description
---

A library that provides ways of composing multimedia projects, in all the spirit of Haskell – i.e. referentially transparent and with no hard-to-trace mutations. All modern nonlinear editors are "non-destructive" in that they don't mutate existing source material; still in most of these programs a lot of operations rely on step-by-step "tuning" projects, rather than defining them precisely correct right away. The latter certainly isn't completely possible in general, but is particularly impeded by the lack of expressiveness in typical GUIs. (That is not to say a mutable workflow is necessarily bad for such programs; there sure are great nonlinear editors / digital audio workstations around which work this way. But there may be room for improvement!)

Unlike many other media libraries (e.g. [`dsp`](http://hackage.haskell.org/package/dsp)) this one follows a very "physical", "analogue" style, for instance the notion of an audio sample rate only appears at the very lowest level. On the higher levels all time-dependent data is thought of as just this: some kind of quantity (be it sound pressure, brightness distribution or some control parameter) that varies _as a function of time_. For that's what you're really interested in when mixing music or cutting a film: the eventual time-continous, analogue results; you don't really want to be bothered with any details of the digital implementation.

Since Haskell is a functional language, a naïve version of this approach is of course trivially simple to implement: something like `type Audio = Double->Double`. Unfortunately, that fails to be useful in any really interesting project; particularly audio effects (all but the most trivial) require not just one time-value of its input function to calculate one output value but a whole range (every filter and many other effects as well are some kind of _convolution_). Calculating all of these over and over again becomes infeasible very quickly. `Media.Timed.Timeline` solves the problem by yielding not just a single time-value when requested, but also a continuation for calculating subsequent values with reasonable efficiency. Such a time-rendering is in some ways like the lazy lists used in the [`dsp`](http://hackage.haskell.org/package/dsp) package, but here not just samples or video frames are yielded in each continuation step but whole _strictly evaluated chunks_, which is like the buffers all common audio programs also use. This improves CPU cache usage and also gives more flexibility: while the length of the chunks can stay fixed to ensure proper alignment, parameters such as sample rate or frame resolution stay in the low-level rendering domain and may well be adjusted dynamically, to suit quality demand and processor availability.

Features
---

The library has at the moment only the very most basic features: editing of mono audio, which can be loaded from simple `.wav` files or internally generated from pure functions, processed with basic lowpass filters and gain controls, and plainly mixed down. Output is currently possible only by piping to the (linux/ALSA) `aplay` command.

The infrastructure should however allow adding much more advances features in quite a straightforward manner.


Use cases / future directions
---

The main intended use (once the necessary features have been added) is as the core library for a nonlinear video editing program. Within that scope, audio processing is heavily emphasised, but not so much sequencing / looping / live performance (though the latter would be desirable, but Haskell's nondeterministic garbage collector makes it virtually impossible to guarantee safe performance at low latencies) as arranging and mixing down of prerecorded audio tracks, perhaps also with MIDI-controlled instruments etc..

The general philosophy of keeping as much as possible of the digital implementation hidden (including time-limits of files) and of holding to Haskell's immutable-style implies that the program should follow a workflow rather different from the usual. Instead of pushing around "events" on a timeline, the preferred method might be _annotating_ the source material (itself consisting of infinite timelines) with "anchors" and having the program figure out by itself how to align the material so the anchors match.

There shall at no level be any such thing as a fixed time-raster. Time is to be kept strictly apart from musical metric. While a raster is typically found in software aiming at music production, it is IMO (except for a few special cases) very _un-musical_ to impose any kind of constraint on the performance tempo (or other performance parameters). An anchor-approach should allow to keep all musical expressiveness, and still allow doing tasks that normally require a raster, such as combining independently recorded tracks, adding a click track to existing audio to aid orientation, perhaps even combining recorded tracks with notation material.

Installing
---

Everything should set up fine by using the ordinary `cabal install`.