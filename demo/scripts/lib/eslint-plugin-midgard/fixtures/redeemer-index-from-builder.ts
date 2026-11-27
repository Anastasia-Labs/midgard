declare const lucid: any;
declare const Data: any;
declare const StateQueueRedeemer: unknown;
declare const Step02SpendRedeemer: unknown;
declare const ForcedOrderRedeemer: unknown;
declare const assets: Record<string, bigint>;
declare const thread: UTxO;
declare const carriage: UTxO;
declare const stepReference: UTxO;
declare const carriageUtxos: readonly UTxO[];
declare const inputs: readonly UTxO[];
declare const planned: unknown;
declare const plan: unknown;
declare const outputMatches: (output: unknown) => boolean;
declare const requireInputIndex: (
  ctx: unknown,
  utxo: UTxO,
  label: string,
) => bigint;
declare const requireUniqueOutputIndex: (
  outputs: unknown,
  predicate: unknown,
  label: string,
) => bigint;
declare const faultProofFieldOpening: (options: object) => unknown;
declare const resolveChunkReferenceIndices: (options: object) => bigint[];
declare const submitLinearFaultFinalize: (options: object) => unknown;
type UTxO = { txHash: string; outputIndex: number };
type RedeemerContext = { referenceInputs: readonly UTxO[]; outputs: unknown };

const compareOutRefs = (a: UTxO, b: UTxO) =>
  a.txHash.localeCompare(b.txHash, "en") || a.outputIndex - b.outputIndex;

// The protocol-initialization shape: the state-queue output's position fixed
// as a literal next to the pay calls that happen to put it there.
export const initialize = () =>
  lucid.newTx().mintAssets(
    assets,
    // ruleid: midgard/redeemer-index-from-builder
    Data.to({ InitV1: { output_index: 3n } }, StateQueueRedeemer),
  );

// A spend redeemer encoded before the builder ran.
export const staticSpend = () => {
  const redeemer = Data.to(
    {
      Continue: [
        {
          // ruleid: midgard/redeemer-index-from-builder
          input_index: 0n,
          // ruleid: midgard/redeemer-index-from-builder
          output_index: BigInt(0),
        },
      ],
    },
    Step02SpendRedeemer,
  );
  return lucid.newTx().collectFrom([thread], redeemer);
};

// The sorted-copy shape: the caller sorts its own list and takes a position.
export const sortedPosition = () => {
  const sorted = [...carriageUtxos, stepReference].sort(compareOutRefs);
  return lucid.newTx().collectFrom(
    [thread],
    Data.to(
      // ruleid: midgard/redeemer-index-from-builder
      { RawUtxo: { ref_input_index: BigInt(sorted.indexOf(carriage)) } },
      ForcedOrderRedeemer,
    ),
  );
};

// A sort-and-search through a local constant.
export const searchedPosition = () => {
  const position = [...inputs]
    .sort(compareOutRefs)
    .findIndex((utxo) => utxo.txHash === thread.txHash);
  return lucid.newTx().collectFrom(
    [thread],
    Data.to(
      {
        Continue: [
          {
            // ruleid: midgard/redeemer-index-from-builder
            input_index: BigInt(position),
          },
        ],
      },
      Step02SpendRedeemer,
    ),
  );
};

// The shape the carriage fix replaced: the opening resolved against a list
// assembled next to readFrom, so a reference input the builder adds or the
// list omits shifts every index.
export const handListOpening = () => {
  const opening = faultProofFieldOpening({
    planned,
    // ruleid: midgard/redeemer-index-from-builder
    referenceInputs: [...carriageUtxos, stepReference],
  });
  return lucid
    .newTx()
    .readFrom([...carriageUtxos, stepReference])
    .collectFrom([thread], () => Data.to({ opening }, Step02SpendRedeemer));
};

// Inside a callback is not enough: the list itself must be the builder's.
export const handListInsideCallback = () => {
  const referenceInputs = [...carriageUtxos, stepReference];
  return lucid.newTx().collectFrom([thread], (ctx: RedeemerContext) =>
    Data.to(
      {
        input_index: requireInputIndex(ctx, thread, "thread"),
        chunk_ref_input_indices: resolveChunkReferenceIndices({
          plan,
          // ruleid: midgard/redeemer-index-from-builder
          referenceInputs,
        }),
      },
      Step02SpendRedeemer,
    ),
  );
};

export const fromContext = () =>
  lucid.newTx().collectFrom([thread], (ctx: RedeemerContext) =>
    Data.to(
      {
        Continue: [
          {
            // ok: midgard/redeemer-index-from-builder
            input_index: requireInputIndex(ctx, thread, "thread"),
            // ok: midgard/redeemer-index-from-builder
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "thread",
            ),
            opening: faultProofFieldOpening({
              planned,
              // ok: midgard/redeemer-index-from-builder
              referenceInputs: ctx.referenceInputs,
            }),
          },
        ],
      },
      Step02SpendRedeemer,
    ),
  );

// A destructured builder context is still the builder's.
export const destructuredContext = () =>
  lucid.newTx().collectFrom([thread], ({ referenceInputs }: RedeemerContext) =>
    Data.to(
      {
        opening: faultProofFieldOpening({
          planned,
          // ok: midgard/redeemer-index-from-builder
          referenceInputs,
        }),
      },
      Step02SpendRedeemer,
    ),
  );

// A literal inside the builder callback names a position the callback has
// the final transaction to check against.
export const literalInCallback = () =>
  lucid.newTx().mintAssets(assets, (_ctx: RedeemerContext) =>
    // ok: midgard/redeemer-index-from-builder
    Data.to({ InitV1: { output_index: 0n } }, StateQueueRedeemer),
  );

// Lucid's selected-input `RedeemerBuilder` hands `makeRedeemer` the final
// indices of the inputs it names.
export const selectedInputs = () =>
  lucid.newTx().mintAssets(assets, {
    kind: "selected",
    inputs: [thread],
    makeRedeemer: (indices: bigint[]) =>
      Data.to(
        {
          AuthenticateEvent: {
            // ok: midgard/redeemer-index-from-builder
            nonce_input_index: indices[0],
          },
        },
        StateQueueRedeemer,
      ),
  });

// The family-arguments shape: the layout and the context come from the
// shared finalizer's redeemer callback.
export const familyArgs = () =>
  submitLinearFaultFinalize({
    buildFamilyArgs: (
      { input_index }: { input_index: bigint },
      ctx: RedeemerContext,
    ) => ({
      input_index,
      opening: faultProofFieldOpening({
        planned,
        // ok: midgard/redeemer-index-from-builder
        referenceInputs: ctx.referenceInputs,
      }),
    }),
  });

// A resolver parameter passed through unchanged: the caller supplies the
// builder's list.
export const openingFor = (referenceInputs: readonly UTxO[]) =>
  faultProofFieldOpening({
    planned,
    // ok: midgard/redeemer-index-from-builder
    referenceInputs,
  });

export const outputReference = () =>
  lucid.newTx().collectFrom(
    [thread],
    Data.to(
      {
        anchor: {
          transaction_id: thread.txHash,
          // ok: midgard/redeemer-index-from-builder
          output_index: 0n,
        },
        // ok: midgard/redeemer-index-from-builder
        field_index: 2n,
      },
      Step02SpendRedeemer,
    ),
  );
