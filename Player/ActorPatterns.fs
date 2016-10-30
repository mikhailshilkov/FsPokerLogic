namespace Player

module ActorPatterns =
  open Akka.FSharp
  open Akka.Actor

  let actorOfSink f =
    actorOf2 (fun _ msg -> f msg)

  let actorOfStatefulSink f initialState (mailbox : Actor<'a>) =

    let rec imp lastState =
      actor {
        let! msg = mailbox.Receive()
        let newState = f msg lastState
        return! imp newState
      }

    imp initialState


  let actorOfConvert f outputRef =
    actorOf2 (fun _ msg -> outputRef <! f msg)

  let actorOfStatefulConvert f initialState (outputRef1, outputRef2) (mailbox : Actor<'a>) =

    let rec imp lastState =
      actor {
        let! msg = mailbox.Receive()
        let (result, newState) = f msg lastState
        match result with
        | Some (r1, r2) ->
          outputRef1 <! r1
          outputRef2 <! r2
        | _ -> ()
        return! imp newState
      }

    imp initialState

  let actorOfConvertToChild f spawnChild (mailbox : Actor<'a>) =

    let rec imp state =
      actor {
        let newstate =
          match state with
          | Some s -> s
          | None -> spawnChild mailbox

        let! msg = mailbox.Receive()
        f msg |> Option.iter (fun x -> newstate <! x) 
        return! imp (Some newstate)
      }

    imp None

  let actorOfConvertToChildren f spawnChild (mailbox : Actor<'a>) =

    let getActor id = 
      let actorRef = mailbox.Context.Child(id)
      if actorRef.IsNobody() then
        spawnChild id mailbox
      else 
        actorRef

    let rec imp () =
      actor {
        let! msg = mailbox.Receive()
        f msg |> Seq.iter (fun (id, x) -> (getActor id) <! x) 
        return! imp ()
      }

    imp ()