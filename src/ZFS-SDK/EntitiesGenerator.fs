module EntitiesGenerator

// module Result   = Infrastructure.Result
// 
module Contract = Consensus.Contract
module Types    = Consensus.Types
module Asset    = Consensus.Asset
// module ZFSConv  = Consensus.ZFStar
module Hash     = Consensus.Hash
// module Crypto   = Consensus.Crypto
module Tx       = Consensus.TxSkeleton
// 
// module ZData    = Zen.Types.Data
// module ZExt     = Zen.Types.Extracted

// type fstString   = FStar.String.t
type Hash          = Hash.Hash
type Input         = Tx.Input
type ContractId    = Types.ContractId
type Outpoint      = Types.Outpoint
type PointedOutput = Consensus.Types.PointedOutput
type Output        = Types.Output
type Spend         = Types.Spend
type Asset         = Types.Asset
type Wallet        = List<PointedOutput>
// type filename    = string
// type txSkeleton  = Consensus.TxSkeleton.T
// type message     = Consensus.Types.Message
// type stateUpdate = Zen.Types.Main.stateUpdate
// 
// type contractReturn = txSkeleton * Option<message> * stateUpdate 
// type CR             = Result<contractReturn, string>

type SimpleContract<'contract> =
        | ThisContract
        | OtherContract of 'contract
    
type SimpleLock<'pk, 'contract> =
    | PK       of 'pk
    | Contract of SimpleContract<'contract>

type amount = uint64

type SimpleTxComponent<'pk, 'contract, 'asset> =
    | Mint of 'asset * amount
    | Input of 'asset * amount
    | Output of SimpleLock<'pk, 'contract> * 'asset * amount 

type SimplePointedOutput<'pk, 'contract, 'asset> = SimpleLock<'pk, 'contract> * 'asset * amount
         
type SimpleWallet<'pk, 'contract, 'asset> = List<SimplePointedOutput<'pk, 'contract, 'asset>> 

type GenEnv<'pk, 'contract, 'asset> = {
    hashPKs       : 'pk       -> Hash
    hashContracts : 'contract -> Hash
    realizeAsset  : 'asset    -> Option<Asset>
    thisContract  : ContractId
}

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
========================================================================================================================
f03qi4w-50isdf09giek50496kg0d9gfk9gv8ioalkjwro34iqwraj;psgujtio21q;jaw;ozvfijt98234eujtro9s8iujgf98oiufjhs9do8ajf9of20oi
XSAD:SC(VSOPSFKSOPRK<$ER)SDIK)F(SDXKF()F)S(UI$J)RPK)(RFJK)DPSFJ)(PSEJFP:(OIAJHDP(AUJFP(ASDJ(FD(S*FJU(DSJR$(#()E)(FUSFSID
adsf0o9g=-ol4ker0dgid0sf9gjk9we4356ujt9erp8tguwe90rfuajs9d0fuj943wi5uj9-asudjf908oasudf908adsujf9oi84j3q908rj09af0sa9dif
========================================================================================================================
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
let mkPointedOutput lock asset amount : PointedOutput =
    let outpoint = {Outpoint.txHash=Hash.zero; Outpoint.index=0u}
    (outpoint, mkOutput lock asset amount)

// generate a mint input from the specified asset and amount
let mkMint asset amount : Input =
    Tx.Mint (mkSpend asset amount)

let realizeLock
    (env : GenEnv<'pk, 'contract, 'asset>)
    (lock : SimpleLock<'pk, 'contract>)
    : Types.Lock =
    match lock with
    | PK pk               -> Types.PK <| env.hashPKs pk
    | Contract contract ->
        match contract with
        | ThisContract             -> Types.Contract <| env.thisContract 
        | OtherContract contractId -> Types.Contract <| Types.ContractId (0ul, env.hashContracts contractId)

(*
------------------------------------------------------------------------------------------------------------------------
======================================== TX Generation =================================================================
------------------------------------------------------------------------------------------------------------------------
*)

module TxGeneration =
    
    let stringToHash (s : string) : Hash =
        System.Text.Encoding.ASCII.GetBytes(s) |> Hash.compute
    
    let stringGenEnv (cid : ContractId) : GenEnv<string, string, string> = {
        hashPKs       = stringToHash
        hashContracts = stringToHash
        realizeAsset  = Asset.fromString
        thisContract  = cid
    }
    
    type private FinalizedComponent =
        | FinalizedInput of Input
        | FinalizedOutput of Types.Output
    
    type private SimpleTx = List<FinalizedComponent>
        
    let initTx : SimpleTx = List.empty
    
    let private finalizeComponent
        (env : GenEnv<'pk, 'contract, 'asset>)
        (item : SimpleTxComponent<'pk, 'contract, 'asset>)
        : Option<FinalizedComponent> =
        match item with
        | Mint (asset, amount) -> option {
                let! asset = env.realizeAsset asset
                return FinalizedInput <| mkMint asset amount
            }
        | Input (asset, amount) -> option {
                let! asset = env.realizeAsset asset
                return FinalizedInput <| mkInput (Types.PK zeroHash) asset amount
            }
        | Output (lock, asset, amount) -> option {
                let! asset = env.realizeAsset asset
                return FinalizedOutput <| mkOutput (realizeLock env lock) asset amount
            }
        
    let addSimpleInput (env : GenEnv<'pk, 'contract, 'asset>)
        (asset : 'asset) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent env <| Input (asset, amount) with
        | Some t -> t :: tx
        | None   -> tx
        
    let addSimpleMint (env : GenEnv<'pk, 'contract, 'asset>)
        (asset : 'asset) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent env <| Mint (asset, amount) with
        | Some t -> t :: tx
        | None   -> tx
    
    let addSimpleOutput (env : GenEnv<'pk, 'contract, 'asset>)
        (lock : SimpleLock<'pk, 'contract>) (asset : 'asset) (amount : uint64) (tx : SimpleTx) : SimpleTx =
        match finalizeComponent env <| Output (lock, asset, amount) with
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

module WalletGeneration =
    
    let initWallet : SimpleWallet<'pk, 'contract, 'asset> = List.empty
        
    let add
        (pOutput : SimplePointedOutput<'pk, 'contract, 'asset>)
        (wallet : SimpleWallet<'pk, 'contract, 'asset>)
        : SimpleWallet<'pk, 'contract, 'asset> =
        pOutput :: wallet
    
    let private finalizePointedOutput
        (env : GenEnv<'pk, 'contract, 'asset>)
        ((lock, asset, amount) : SimplePointedOutput<'pk, 'contract, 'asset>)
        : Option<PointedOutput> = option {
            let! asset = env.realizeAsset asset
            return mkPointedOutput (realizeLock env lock) asset amount
        }
    
    let finalize
        (env : GenEnv<'pk, 'contract, 'asset>)
        (wallet : SimpleWallet<'pk, 'contract, 'asset>)
        : Wallet =
        List.choose (finalizePointedOutput env) wallet
