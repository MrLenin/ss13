module Events

open System

open SFML.Graphics
open SFML.Window

open State

let window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let context = new Context (window)
let stateStack = new StateStack (context)

// Window/UI Events
window.Closed.AddHandler (new EventHandler
    (fun sender args ->
    stateStack.HandleEvent (Args args)
    window.Close ()))

window.Resized.AddHandler (new EventHandler<SizeEventArgs> 
    (fun sender args -> stateStack.HandleEvent (Args args)))

window.LostFocus.AddHandler (new EventHandler 
    (fun sender args -> stateStack.HandleEvent (Args args)))

window.GainedFocus.AddHandler (new EventHandler
    (fun sender args -> stateStack.HandleEvent (Args args)))

// Keyboard Events
window.TextEntered.AddHandler (new EventHandler<TextEventArgs>
    (fun sender args -> stateStack.HandleEvent (TextArgs args)))

window.KeyPressed.AddHandler (new EventHandler<KeyEventArgs>
    (fun sender args -> stateStack.HandleEvent (KeyArgs args)))

window.KeyReleased.AddHandler (new EventHandler<KeyEventArgs>
    (fun sender args -> stateStack.HandleEvent (KeyArgs args)))

// Mouse Events
window.MouseMoved.AddHandler (new EventHandler<MouseMoveEventArgs>
    (fun sender args -> stateStack.HandleEvent (MouseMoveArgs args)))

window.MouseButtonPressed.AddHandler (new EventHandler<MouseButtonEventArgs>
(fun sender args -> stateStack.HandleEvent (MouseButtonArgs args)))

window.MouseButtonReleased.AddHandler (new EventHandler<MouseButtonEventArgs>
    (fun sender args -> stateStack.HandleEvent (MouseButtonArgs args)))

window.MouseWheelScrolled.AddHandler (new EventHandler<MouseWheelScrollEventArgs>
    (fun sender args -> stateStack.HandleEvent (MouseWheelScrollArgs args)))

window.MouseEntered.AddHandler (new EventHandler
    (fun sender args -> stateStack.HandleEvent (Args args)))

window.MouseLeft.AddHandler (new EventHandler
    (fun sender args -> stateStack.HandleEvent (Args args)))

// Joystick Events
window.JoystickButtonPressed.AddHandler (new EventHandler<JoystickButtonEventArgs>
    (fun sender args -> stateStack.HandleEvent (JoystickButtonArgs args)))

window.JoystickButtonReleased.AddHandler (new EventHandler<JoystickButtonEventArgs>
    (fun sender args -> stateStack.HandleEvent (JoystickButtonArgs args)))

window.JoystickMoved.AddHandler (new EventHandler<JoystickMoveEventArgs>
    (fun sender args -> stateStack.HandleEvent (JoystickMoveArgs args)))

window.JoystickConnected.AddHandler (new EventHandler<JoystickConnectEventArgs>
    (fun sender args -> stateStack.HandleEvent (JoystickConnectArgs args)))

window.JoystickDisconnected.AddHandler (new EventHandler<JoystickConnectEventArgs>
    (fun sender args -> stateStack.HandleEvent (JoystickConnectArgs args)))