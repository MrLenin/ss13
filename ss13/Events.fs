module Events

open System

open SFML.Graphics
open SFML.Window

open State

let internal window = new RenderWindow (VideoMode (800u, 600u), "ss13")
let private context = new Context (window)
let internal stateStack = new StateStack (context)

// Window/UI Events
window.Closed.AddHandler (new EventHandler
    (fun sender eventArgs ->
        stateStack.HandleEvent (ClosedEvent eventArgs)
        window.Close ()
    ))

window.Resized.AddHandler (new EventHandler<SizeEventArgs> 
    (fun sender eventArgs ->
        stateStack.HandleEvent (ResizedEvent eventArgs)
    ))

window.LostFocus.AddHandler (new EventHandler 
    (fun sender eventArgs ->
        stateStack.HandleEvent (LostFocusEvent eventArgs)
    ))

window.GainedFocus.AddHandler (new EventHandler
    (fun sender eventArgs ->
        stateStack.HandleEvent (GainedFocusEvent eventArgs)
    ))

// Keyboard Events
window.TextEntered.AddHandler (new EventHandler<TextEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (TextEnteredEvent eventArgs)
    ))

window.KeyPressed.AddHandler (new EventHandler<KeyEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (KeyPressedEvent eventArgs)
    ))

window.KeyReleased.AddHandler (new EventHandler<KeyEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (KeyReleasedEvent eventArgs)
    ))

// Mouse Events
window.MouseMoved.AddHandler (new EventHandler<MouseMoveEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseMovedEvent eventArgs)
    ))

window.MouseButtonPressed.AddHandler (new EventHandler<MouseButtonEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseButtonPressedEvent eventArgs)
    ))

window.MouseButtonReleased.AddHandler (new EventHandler<MouseButtonEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseButtonReleasedEvent eventArgs)
    ))

window.MouseWheelScrolled.AddHandler (new EventHandler<MouseWheelScrollEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseWheelScrolledEvent eventArgs)
    ))

window.MouseEntered.AddHandler (new EventHandler
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseEnteredEvent eventArgs)
    ))

window.MouseLeft.AddHandler (new EventHandler
    (fun sender eventArgs ->
        stateStack.HandleEvent (MouseLeftEvent eventArgs)
    ))

// Joystick Events
window.JoystickButtonPressed.AddHandler (new EventHandler<JoystickButtonEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (JoystickButtonPressedEvent eventArgs)
    ))

window.JoystickButtonReleased.AddHandler (new EventHandler<JoystickButtonEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (JoystickButtonReleasedEvent eventArgs)
    ))

window.JoystickMoved.AddHandler (new EventHandler<JoystickMoveEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (JoystickMovedEvent eventArgs)
    ))

window.JoystickConnected.AddHandler (new EventHandler<JoystickConnectEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (JoystickConnectedEvent eventArgs)
    ))

window.JoystickDisconnected.AddHandler (new EventHandler<JoystickConnectEventArgs>
    (fun sender eventArgs ->
        stateStack.HandleEvent (JoystickDisconnectedEvent eventArgs)
    ))