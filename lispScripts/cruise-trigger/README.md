# Cruise-Trigger

The cruise-trigger is similar to an non-adaptive cruise control of a car.
It allows the user to set a cruise speed and maintain that speed automatically without holding any button.
The user can stop in between and resume to the cruise speed at any time.
Double-click accelerates to full speed and maintains it automatically until the user ends it. This can be stop, resume to cruise speed or decrementing full speed manually. 

There are two hardware variants of the cruise-trigger

* `cruise-trigger.lisp` requires three buttons: Increment, decrement, trigger.
* `cruise-trigger-only.lisp` requires only two buttons: Increment und decrement. Either button is the trigger. The action is differentiated by single-click, double-click and hold.

### Feature Overview

* click of the trigger button toggles between
  * resume to last stored speed 
  * store speed and slow down to a stop
* double-click of the trigger button toggles between
  * resume to last stored speed
  * store speed and accelerate to full speed
* holding the increment button accelerates continuously to full speed
* holding the decrement button slows down continuously to a stop

Only the three button variant supports additionally the following actions

* click of the increment button accelerates by `button-step` once
* click of the decrement button slows down by `button-step` once

This allows to gain the exact same speed by counting the clicks of the increment or decrement buttons.


## Ramp/Soak Controller (RSC)

The RSC is needed to ramp the duty-cycle of the VESC controller. 
The buttons only change the `rsc-target` speed, whereas the `rsc-actual` speed is changed by the `rsc-update` function. It either increments or decrements the `rsc-actual` speed by `rsc-step`. Therefore, the function is called every `rsc-update-secs` seconds.

## Button

There are several flags and constants defined to detect `button-press`, `button-hold` and `button-click` with an excepted click-count.
Call `button-update` in the main loop.

Overview of flags and constants for single-click, double-click and hold

```clj
             single-click   double-click       hold 

up           ──┐  ┌──────   ──┐  ┌──┐  ┌─────   ──┐     ┌──
               └──┘           └──┘  └──┘          └─────┘ 
hold-secs      ├────┤         ├────┤├────┤        ├────┤ 
               ┌┐ ┌┐          ┌┐ ┌┐ ┌┐ ┌┐         ┌┐    ┌┐
changed      ──┘└─┘└─────   ──┘└─┘└─┘└─┘└────   ──┘└────┘└─
                     ┌┐                   ┌┐
clicked      ────────┘└──   ──────────────┘└─   ───────────
click-secs        ├──┤           ├──┤  ├──┤
click-count  001111111000   00111111222222200   00111110000
```

## Timer

The Timer implements non-blocking delayed function calls. 
Call `timer-tick` in the main loop.

Example:

```clj
(timer-schedule 1.0 (lambda () {(print "Hello") nil })
```

which outputs "Hello" in one second only once.
