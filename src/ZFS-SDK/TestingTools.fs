module TestingTools

module Result   = Infrastructure.Result

module Contract = Consensus.Contract
module Types    = Consensus.Types
module Asset    = Consensus.Asset
module ZFSConv  = Consensus.ZFStar
module Hash     = Consensus.Hash
module Crypto   = Consensus.Crypto
module Tx       = Consensus.TxSkeleton

module ZData    = Zen.Types.Data
module ZExt     = Zen.Types.Extracted



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Type Definitions ==============================================================
------------------------------------------------------------------------------------------------------------------------
*)

type fstString   = FStar.String.t
type Hash        = Hash.Hash
type Input       = Tx.Input
type Outpoint    = Types.Outpoint
type Output      = Types.Output
type Spend       = Types.Spend
type filename    = string
type txSkeleton  = Consensus.TxSkeleton.T
type message     = Consensus.Types.Message
type stateUpdate = Zen.Types.Main.stateUpdate

type contractReturn = txSkeleton * Option<message> * stateUpdate 
type CR             = Result<contractReturn, string>



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Utils =========================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let stringToHash (s : string) : Hash =
    System.Text.Encoding.ASCII.GetBytes(s) |> Hash.compute 



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Option Builder ================================================================
------------------------------------------------------------------------------------------------------------------------
*)

type OptionBuilder() =

    member this.Return<'a> (x : 'a) : 'a option =
        Some x

    member this.Bind<'a, 'b> (m : 'a option, f : 'a -> 'b option) : 'b option =
        Option.bind f m

    member this.ReturnFrom<'a> (opt : 'a option) : 'a option =
        opt

    member this.Zero() =
        None

    member this.Yield<'a> (x : 'a) : 'a option =
        Some x

    member this.YieldFrom<'a> (opt : 'a option) : 'a option =
        opt

    member this.Delay (f : unit -> 'a option) = f()
    
    member this.Combine (opt1 : 'a option , opt2 : 'a option) : 'a option =
        Option.bind (fun _ -> opt2) opt1

let option = OptionBuilder()

let (<@|) f x = Option.map f x
let (|@>) x f = Option.map f x
let (=<<) f x = Option.bind f x
let (>>=) x f = Option.bind f x
let (<*|) f x = Option.bind (fun x -> Option.map (fun f -> f x) f) x
let (|*>) x f = Option.bind (fun x -> Option.map (fun f -> f x) f) x
let sequenceA xs = Infrastructure.Option.traverseA id xs



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Test Result (Success/Failure Conditions) ======================================
------------------------------------------------------------------------------------------------------------------------
*)

type WrongError = {
    expected : string
    given    : string
}

type SuccessCondition =
    | ExpectedOk
    | ExpectedError of string

type FailureCondition =
    | UnexpectedOk
    | UnexpectedError of string
    | WrongError      of WrongError
    | WrongOk         of string

type TestResult =
    | TestSuccess of SuccessCondition
    | TestFailure of FailureCondition

type predicate<'a> = 'a -> bool

type contractReturnValidation = {
    tx_validation      : predicate< txSkeleton >
    message_validation : predicate< Option<message> >
    state_validation   : predicate< stateUpdate >
}

type condition<'a> = {
    message : string
    check   : predicate<'a>
}

let validateContractReturn (conds : contractReturnValidation) : predicate<contractReturn> =
    fun (tx, msg, st) -> conds.tx_validation tx && conds.message_validation msg && conds.state_validation st  

let validateCR (expected : Result<condition<contractReturn>, Option<string>>) (given : CR) : TestResult =
    match expected, given with
        | Ok _, Error msg ->
            TestFailure <| UnexpectedError msg
        | Error _, Ok _ ->
            TestFailure UnexpectedOk
        | Ok e, Ok g ->
            if e.check g
                then TestSuccess <| ExpectedOk
                else TestFailure <| WrongOk e.message
        | Error eo, Error g ->
            match eo with
            | None ->
                TestSuccess <| ExpectedError g
            | Some e ->
                if e = g
                    then TestSuccess <| ExpectedError g
                    else TestFailure <| WrongError { expected = e; given = g }

//type TxFailure =
//    | MissingInput  of (Types.Lock option * string option * uint64 option)
//    | MissingOutput of (Types.Lock option * string option * uint64 option)
//    | MissingMint   of (string option * uint64 option) 
//
//type TestFailure =
//    | ExecutionFailure of string
//    | TxFailure        of TxFailure list



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Contract Loading & Execution ==================================================
------------------------------------------------------------------------------------------------------------------------
*)

let extractMainAndCost (contractDLL : filename) =
    contractDLL
    |> System.Reflection.Assembly.LoadFrom
    |> Contract.getFunctions
    |> Result.get
    
let getRawCode contractSrc = System.IO.File.ReadAllText contractSrc
 
let computeContractId (contractSrc : filename) =
    contractSrc
    |> getRawCode
    |> Contract.makeContractId Types.Version0

let getContractHash (Types.ContractId (_, contractHash)) = contractHash

let getContractVersion (Types.ContractId (contractVersion, _)) = contractVersion



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Randomization =================================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== Reporting =====================================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== Cryptography ==================================================================
------------------------------------------------------------------------------------------------------------------------
*)

let private SerializedPublicKeyLength = 33

let private context = Crypto.Native.secp256k1_context_create (Crypto.Native.SECP256K1_CONTEXT_SIGN ||| Crypto.Native.SECP256K1_CONTEXT_VERIFY)

let serialize (Crypto.PublicKey publicKey) =
    let bytes = Array.create SerializedPublicKeyLength 0uy
    let mutable length = int64 SerializedPublicKeyLength

    match Crypto.Native.secp256k1_ec_pubkey_serialize(context, bytes, &&length, publicKey, Crypto.Native.SECP256K1_EC_COMPRESSED) with
    | Crypto.Native.Result.Ok ->
        if 33L = length then bytes else failwith "Wrong serialized size"
    | _ -> failwith "failed to serialize public key"

let compressPK : ZExt.publicKey -> byte[] =
    Crypto.PublicKey >> serialize

let hashBytes : byte[] -> byte[] =
    Hash.compute >> Hash.bytes



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Data (Message Body & State) Generation ========================================
------------------------------------------------------------------------------------------------------------------------
*)

let emptyDict = Zen.Dictionary.empty

let addToDict (key, value) dict =
    dict
    |> Zen.Dictionary.add key value 
    |> ZFSConv.unCost

let addToDictWith (k : 'a -> ZData.data) (key : fstString) (value : 'a) =
    addToDict (key, k value)

let add_int64     = addToDictWith ZData.I64
let add_byte      = addToDictWith ZData.Byte
let add_byteArray = addToDictWith ZData.ByteArray
let add_uint32    = addToDictWith ZData.U32
let add_uint64    = addToDictWith ZData.U64
let add_string    = addToDictWith (ZFSConv.fsToFstString >> ZData.String)
let add_hash      = addToDictWith ZData.Hash
let add_lock      = addToDictWith ZData.Lock
//let addSignature  = TODO: implement this
let addPublicKey  = addToDictWith (ZFSConv.fsToFstPublicKey >> ZData.PublicKey)
//let addArray      = TODO: implement this
//let addDict       = TODO: implement this
//let addList       = TODO: implement this

let lockUncompressedPK = ZFSConv.fsToFstPublicKey >> compressPK >> hashBytes >> ZExt.PKLock
let lockContract       = ZExt.ContractLock
let lockDestory        = ZExt.DestroyLock

let generateKeyPair    = Crypto.KeyPair.create
let generatePrivateKey = generateKeyPair >> fst
let generatePublicKey  = generateKeyPair >> snd


(*
------------------------------------------------------------------------------------------------------------------------
======================================== Context Generation ============================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== Sender Generation =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== TX Generation =================================================================
------------------------------------------------------------------------------------------------------------------------
*)

// Zero Hash
let zeroHash = Hash.zero

// Empty transaction
let emptyTx : Tx.T = Tx.empty

// Empty messageBody
let emptyMessageBody: Option<Zen.Types.Data.data> = None

// Empty wallet
let emptyWallet: Contract.ContractWallet = []

// generate a TxSkeleton from pointed inputs and outputs
let mkTx pInputs outputs = {Tx.pInputs=pInputs; Tx.outputs=outputs}

// generates a spend from the specified asset and amount
let mkSpend asset amount = {Spend.asset=asset; Spend.amount=amount}

// generate an output from a lock, asset, and amount
let mkOutput lock asset amount : Output =
    {lock=lock; spend=mkSpend asset amount}

// generate an input from an output
let mkInput lock asset amount : Input =
    let outpoint = {Outpoint.txHash=Hash.zero; Outpoint.index=0u}
    Tx.PointedOutput (outpoint, mkOutput lock asset amount)

// generate a pointed output from an output
let mkPointedOutput lock asset amount : Consensus.Types.PointedOutput =
    let outpoint = {Outpoint.txHash=Hash.zero; Outpoint.index=0u}
    (outpoint, mkOutput lock asset amount)

// generate a mint input from the specified asset and amount
let mkMint asset amount : Input =
    Tx.Mint (mkSpend asset amount)

(* ===== Simplified TX generation ==== *)
module SimpleTx =
    
    type SimpleLock =
        | PK of string
        | Contract of string
    
    type SimpleTxComponent =
        | Mint of string * uint64
        | Input of string * uint64
        | Output of SimpleLock * string * uint64 
      
    type private FinalizedComponent =
        | FinalizedInput of Input
        | FinalizedOutput of Types.Output
        
    type private SimpleTx = List<FinalizedComponent>
    
    let initTx = List.empty
    
    let private finalizeLock (lock : SimpleLock) : Types.Lock =
        match lock with
        | PK pk               -> Types.PK       <| stringToHash pk
        | Contract contractId -> Types.Contract <| Types.ContractId (0ul, stringToHash contractId)
    
    let private finalizeComponent (item : SimpleTxComponent) : Option<FinalizedComponent> =
        match item with
        | Mint (asset, amount) -> option {
                let! asset = Asset.fromString asset
                return FinalizedInput <| mkMint asset amount
            }
        | Input (asset, amount) -> option {
                let! asset = Asset.fromString asset
                return FinalizedInput <| mkInput (Types.PK zeroHash) asset amount
            }
        | Output (lock, asset, amount) -> option {
                let! asset = Asset.fromString asset
                return FinalizedOutput <| mkOutput (finalizeLock lock) asset amount
            }
        
    let addSimpleInput (asset : string) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent <| Input (asset, amount) with
        | Some t -> t :: tx
        | None   -> tx
        
    let addSimpleMint (asset : string) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent <| Mint (asset, amount) with
        | Some t -> t :: tx
        | None   -> tx
    
    let addSimpleOutput (lock : SimpleLock) (asset : string) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent <| Output (lock, asset, amount) with
        | Some t -> t :: tx
        | None   -> tx

    let addInput (input : Input) (tx : SimpleTx) : SimpleTx =
        FinalizedInput input :: tx
    
    let addOutput (output : Types.Output) (tx : SimpleTx) : SimpleTx =
        FinalizedOutput output :: tx
    
    let private organizeTx : SimpleTx -> list<Input> * list<Types.Output> =
        let rec organizeTxAux ((ins, outs) : list<Input> * list<Types.Output>) (tx : SimpleTx) : list<Input> * list<Types.Output> =
            match tx with
            | [] ->
                (ins, outs)
            | hd :: tl ->
                match hd with
                | FinalizedInput input   -> organizeTxAux (input::ins, outs        ) tl
                | FinalizedOutput output -> organizeTxAux (ins       , output::outs) tl
        in organizeTxAux ([], [])
    
    let finalize (tx : SimpleTx) : Tx.T =
        let (inps, outs) = organizeTx tx in {
            pInputs = inps
            outputs = outs
        }



(*
------------------------------------------------------------------------------------------------------------------------
======================================== Wallet Generation =============================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== TX Validation =================================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this

(*
------------------------------------------------------------------------------------------------------------------------
======================================== Data Validation ===============================================================
------------------------------------------------------------------------------------------------------------------------
*)

// TODO: implement this