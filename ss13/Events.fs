module Events

open System

open SFML.Graphics
open SFML.Window

open State

let window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let context = new Context (window)
let stateStack = new StateStack (context)

// Window/UI Events
let private OnClose sender args =
    stateStack.HandleEvent (Args args)
    window.Close ()

let private OnResized sender (args : SizeEventArgs) =
    stateStack.HandleEvent (SizeArgs args)

let private OnLostFocus sender args =
    stateStack.HandleEvent (Args args)

let private OnGainedFocus sender args =
    stateStack.HandleEvent (Args args)

window.Closed.AddHandler (new EventHandler (OnClose))
window.Resized.AddHandler (new EventHandler<SizeEventArgs> (OnResized))
window.LostFocus.AddHandler (new EventHandler (OnLostFocus))
window.GainedFocus.AddHandler (new EventHandler (OnGainedFocus))

// Keyboard Events
let private OnTextEntered sender (args : TextEventArgs) =
    stateStack.HandleEvent (TextArgs args)

let private OnKeyPressed sender (args : KeyEventArgs) =
    stateStack.HandleEvent (KeyArgs args)

let private OnKeyReleased sender (args : KeyEventArgs) =
    stateStack.HandleEvent (KeyArgs args)

window.TextEntered.AddHandler (new EventHandler<TextEventArgs> (OnTextEntered))
window.KeyPressed.AddHandler (new EventHandler<KeyEventArgs> (OnKeyPressed))
window.KeyReleased.AddHandler (new EventHandler<KeyEventArgs> (OnKeyReleased))

// Mouse Events
let private OnMouseMoved sender (args : MouseMoveEventArgs) =
    stateStack.HandleEvent (MouseMoveArgs args)

let private OnMouseButtonPressed sender (args : MouseButtonEventArgs) =
    stateStack.HandleEvent (MouseButtonArgs args)

let private OnMouseButtonReleased sender (args : MouseButtonEventArgs) =
    stateStack.HandleEvent (MouseButtonArgs args)

let private OnMouseWheelScrolled sender (args : MouseWheelScrollEventArgs) =
    stateStack.HandleEvent (MouseWheelScrollArgs args)

let private OnMouseEntered sender (args : System.EventArgs) =
    stateStack.HandleEvent (Args args)
    
let private OnMouseLeft sender (args : System.EventArgs) =
    stateStack.HandleEvent (Args args)

window.MouseMoved.AddHandler (new EventHandler<MouseMoveEventArgs> (OnMouseMoved))
window.MouseButtonPressed.AddHandler (new EventHandler<MouseButtonEventArgs> (OnMouseButtonPressed))
window.MouseButtonReleased.AddHandler (new EventHandler<MouseButtonEventArgs> (OnMouseButtonReleased))
window.MouseWheelScrolled.AddHandler (new EventHandler<MouseWheelScrollEventArgs> (OnMouseWheelScrolled))
window.MouseEntered.AddHandler (new EventHandler (OnMouseEntered))
window.MouseLeft.AddHandler (new EventHandler (OnMouseLeft))

// Joystick Events
let private OnJoystickButtonPressed sender (args : JoystickButtonEventArgs) =
    stateStack.HandleEvent (JoystickButtonArgs args)

let private OnJoystickButtonReleased sender (args : JoystickButtonEventArgs) =
    stateStack.HandleEvent (JoystickButtonArgs args)

let private OnJoystickMoved sender (args : JoystickMoveEventArgs) =
    stateStack.HandleEvent (JoystickMoveArgs args)

let private OnJoystickConnected sender (args : JoystickConnectEventArgs) =
    stateStack.HandleEvent (JoystickConnectArgs args)
    
let private OnJoystickDisconnected sender (args : JoystickConnectEventArgs) =
    stateStack.HandleEvent (JoystickConnectArgs args)

window.JoystickButtonPressed.AddHandler (new EventHandler<JoystickButtonEventArgs> (OnJoystickButtonPressed))
window.JoystickButtonReleased.AddHandler (new EventHandler<JoystickButtonEventArgs> (OnJoystickButtonReleased))
window.JoystickMoved.AddHandler (new EventHandler<JoystickMoveEventArgs> (OnJoystickMoved))
window.JoystickConnected.AddHandler (new EventHandler<JoystickConnectEventArgs> (OnJoystickConnected))
window.JoystickDisconnected.AddHandler (new EventHandler<JoystickConnectEventArgs> (OnJoystickDisconnected))