Ok, this is great, but let's adjust how we're thinking about this.

(ignoring sampling and sequencing for now)
Logically, our DIY synth can be composed of knobs, devices and the output. The user has two forms of input: turning knobs or modifying connections (between knobs->devices, devices->devices and devices->output). 

Knobs could have a value of 0.0 to 1.0 (but would need to be converted by default to usable values in many cases) 

Devices include things like:

- audio oscillators
- low-frequency oscillators
- modulators
- filters
- reverb
- delay
- chorus
- mixers


Each device would have inputs and an output.

For example, an audio oscillator device might actually operate up to three oscillators.

It would have inputs like:
- amplitude
- base frequency
- waveform
- relative note/octave offsets for 2nd and 3rd oscillators
- detune
- portamento

A filter might take inputs:
- input signal (audio signal)
- type (low-pass, high-pass)
- frequency / cutoff
- peak / resonance

A LFO could take inputs:
- frequency
- amplitude
- waveform


In order to setup a configurable tremolo, the user (me) might:

- map knob 1 to lfo0.frequency
- map knob 2 to lfo0.amplitude
- map lfo0.output and knob 0 to the inputs of mixer0
- map mixer0.output to oscillator0.amplitude
- map oscillator0.output to OUT

- adjust knobs 0 and 2 to get the desired average volume and tremolo effect

---

This composable architecture would be extremely flexible. But, how much of it can Overtone/SuperCollider produce? And how should it be coded?

Does this make sense?
