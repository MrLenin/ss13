namespace ss13

open System

type Fifo<'a when 'a : equality> = 
    new () = { xs = []; rxs = [] }
    new (xs, rxs) = { xs = xs; rxs = rxs }

    val xs : 'a list;
    val rxs : 'a list;

    static member Empty () = new Fifo<'a> ()
    member q.IsEmpty = (q.xs = []) && (q.rxs = [])
    member q.Enqueue(x) = Fifo(q.xs,x::q.rxs)
    member q.Take() = 
        if q.IsEmpty then failwith "fifo.Take: empty queue"
        else match q.xs with
                | [] -> (Fifo(List.rev q.rxs,[])).Take()
                | y::ys -> (Fifo(ys, q.rxs)),y

[<Flags>]
type Category =
    | None = 0
    | Scene = 1

type CommandImpl<'a> = delegate of 'a * float -> unit

type Command<'a> = {
    Action : CommandImpl<'a>
    Category : Category
    }