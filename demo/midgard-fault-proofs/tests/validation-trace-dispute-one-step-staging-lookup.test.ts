/** Where our journal shows a staged step's operands staged, as our plan
 * reads it (one-step-staging.ts `lookup`): of the live staging outputs at
 * their confirmation depth, those a resolve move of ours read first, and,
 * past the PrepareSelected move, the staging whose evidence that move
 * committed, from our journal when it is no longer live; and a wait on the
 * L1 source when it cannot say how deep a staging output stands. */
import {
  midgardCekStagingAddress,
  midgardCekStagingResult,
  planMidgardCekStaging,
} from "@al-ft/midgard-sdk";
import {
  CML,
  generateEmulatorAccount,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DISPUTE_ACTION } from "../src/validation-trace-dispute/actions.js";
import { validationTraceStaging } from "../src/validation-trace-dispute/one-step-staging.js";
import { VALIDATION_TRACE_STAGING } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import {
  FraudProofL1UnavailableError,
  type FraudProofSignedTransactionRecovery,
} from "../src/workflow/l1-source.js";

const ours = generateEmulatorAccount({ lovelace: 0n });
const KEY = paymentCredentialOf(ours.address).hash;
const STAGING = midgardCekStagingAddress("Custom", KEY);

/** A staged operand of several runs. */
const OPERANDS = [
  Buffer.from(Array.from({ length: 30_000 }, (_, i) => (i * 31 + 7) & 0xff)),
];
const RUNS = planMidgardCekStaging(OPERANDS);

const input = (seed: number) =>
  CML.TransactionInput.new(
    CML.TransactionHash.from_hex(seed.toString(16).padStart(64, "0")),
    0n,
  );

const outRefInput = (outRef: string) => {
  const [hash, index] = outRef.split("#");
  return CML.TransactionInput.new(
    CML.TransactionHash.from_hex(hash!),
    BigInt(index!),
  );
};

/** A transaction spending the made-up input `seed`, reading `reads`, and
 * paying one output to `address`, holding `datum` when given. */
const transaction = ({
  seed,
  reads = [],
  address = ours.address,
  datum,
}: {
  readonly seed: number;
  readonly reads?: readonly string[];
  readonly address?: string;
  readonly datum?: string;
}) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(input(seed));
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
      datum === undefined
        ? undefined
        : CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(datum)),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
  if (reads.length > 0) {
    const references = CML.TransactionInputList.new();
    for (const outRef of reads) references.add(outRefInput(outRef));
    body.set_reference_inputs(references);
  }
  const cbor = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  ).to_cbor_hex();
  return { cbor, hash: CML.hash_transaction(body).to_hex() };
};

const confirmed = (
  actionId: string,
  actionInput: Record<string, unknown>,
  { hash }: { hash: string },
  durableRecovery: Record<string, unknown>,
) => [
  {
    event: {
      kind: "submission_intent",
      actionId,
      actionInput,
      durableRecovery,
      attempt: 1,
      txHash: hash,
    },
  },
  { event: { kind: "confirmed", actionId, txHash: hash } },
];

/** One staging of every run, the `generation`th, from made-up inputs
 * `seed` on: its transactions and its staging outputs in run order. */
const stagingOf = (generation: number, seed: number) => {
  const staged = RUNS.map(({ datumCbor }, run) => ({
    datumCbor,
    tx: transaction({ seed: seed + run, address: STAGING, datum: datumCbor }),
  }));
  return {
    outRefs: staged.map(({ tx }) => `${tx.hash}#0`),
    entries: staged.flatMap(({ datumCbor, tx }, run) =>
      confirmed(
        `validation-trace-stage:${datumCbor.slice(0, 8)}${run.toString()}:${generation.toString()}`,
        {
          schemaVersion: VALIDATION_TRACE_STAGING,
          category: "validationTraceDispute",
          stage: "stage_operands",
          run: `${datumCbor.slice(0, 8)}${run.toString()}`,
        },
        tx,
        { signedTransactionCborHex: tx.cbor, outRef: `${tx.hash}#0` },
      ),
    ),
  };
};

/** A confirmed resolve move of ours reading `reads`. */
const readerOf = (reads: readonly string[], seed: number) => {
  const tx = transaction({ seed, reads });
  return confirmed(
    `validation-trace-resolve:${seed.toString()}`,
    {
      schemaVersion: VALIDATION_TRACE_DISPUTE_ACTION,
      category: "validationTraceDispute",
      stage: "resolve",
      consumes: `${"77".repeat(32)}#${seed.toString()}`,
    },
    tx,
    { validationTrace: { signedTransactionCborHex: tx.cbor } },
  );
};

const first = stagingOf(0, 0x100);
const second = stagingOf(1, 0x200);
const datumOf = new Map(
  [first, second].flatMap(({ outRefs }) =>
    outRefs.map((outRef, run) => [outRef, RUNS[run]!.datumCbor] as const),
  ),
);

/** The staging naming the outputs `outRefs`, one a run. */
const resultOn = (outRefs: readonly string[]) =>
  midgardCekStagingResult(
    OPERANDS,
    RUNS,
    outRefs.map((outRef) => {
      const [hash, index] = outRef.split("#");
      return {
        transactionId: Buffer.from(hash!, "hex"),
        outputIndex: Number(index),
      };
    }),
  );

/** Our lookup over a journal holding both stagings, each read by a resolve
 * move of ours (the second the later), with `live` the outputs the ledger
 * still holds and `observe` where each staging transaction stands. */
const lookupOver = ({
  live = [...first.outRefs, ...second.outRefs],
  observe = async () => ({
    status: "included" as const,
    inclusionPoint: { blockNo: "1" },
    canonicalPoint: { blockNo: "100" },
    releaseFinalPoint: { blockNo: "100" },
  }),
}: {
  readonly live?: readonly string[];
  readonly observe?: () => Promise<unknown>;
} = {}) => {
  const lucid = {
    config: () => ({ network: "Custom" }),
    utxosByOutRef: async (
      refs: readonly { txHash: string; outputIndex: number }[],
    ): Promise<UTxO[]> =>
      refs.flatMap(({ txHash, outputIndex }) => {
        const outRef = `${txHash}#${outputIndex.toString()}`;
        return live.includes(outRef)
          ? [
              {
                txHash,
                outputIndex,
                address: STAGING,
                assets: { lovelace: 2_000_000n },
                datum: datumOf.get(outRef)!,
              },
            ]
          : [];
      }),
  };
  const staging = validationTraceStaging({
    lucid: lucid as never,
    signer: { paymentKeyHash: KEY, address: ours.address } as never,
    deployment: {} as never,
    signedRecovery: {
      observeSignedTransaction: observe,
    } as unknown as FraudProofSignedTransactionRecovery,
    materialOf: () => undefined,
    recent: () => false,
  });
  return staging.lookup({
    entries: [
      ...first.entries,
      ...readerOf(first.outRefs, 1),
      ...second.entries,
      ...readerOf(second.outRefs, 2),
    ],
  } as never);
};

describe("where our journal shows a staged step's operands staged", () => {
  it("names the staging the latest resolve move of ours read", async () => {
    expect(RUNS.length).toBeGreaterThan(1);
    expect(await lookupOver()(OPERANDS)).toEqual(resultOn(second.outRefs));
  });

  it("names, past the PrepareSelected move, the staging another resolve move of ours read when that is the one its evidence names", async () => {
    const named = JSON.stringify(resultOn(first.outRefs));
    const accepts = (staging: unknown) => JSON.stringify(staging) === named;
    expect(await lookupOver()(OPERANDS, accepts)).toEqual(
      resultOn(first.outRefs),
    );
    // Evidence no staging matches names the latest.
    expect(await lookupOver()(OPERANDS, () => false)).toEqual(
      resultOn(second.outRefs),
    );
  });

  it("names, past the PrepareSelected move, the staging its evidence names from our journal once an output of it is no longer live, and no other", async () => {
    const named = (outRefs: readonly string[]) => {
      const text = JSON.stringify(resultOn(outRefs));
      return (staging: unknown) => JSON.stringify(staging) === text;
    };
    for (const live of [[...first.outRefs.slice(1), ...second.outRefs], []]) {
      expect(
        await lookupOver({ live })(OPERANDS, named(first.outRefs)),
      ).toEqual(resultOn(first.outRefs));
      expect(
        await lookupOver({ live })(OPERANDS, named(second.outRefs)),
      ).toEqual(resultOn(second.outRefs));
    }
    // Evidence neither staging matches names none of them once none is
    // live.
    expect(
      await lookupOver({ live: [] })(OPERANDS, () => false),
    ).toBeUndefined();
  });

  it("waits on the L1 source when it cannot say how deep a staging output stands", async () => {
    const unavailable = new FraudProofL1UnavailableError(
      "the follower is not ready",
    );
    await expect(
      lookupOver({ observe: () => Promise.reject(unavailable) })(OPERANDS),
    ).rejects.toBe(unavailable);
  });
});
