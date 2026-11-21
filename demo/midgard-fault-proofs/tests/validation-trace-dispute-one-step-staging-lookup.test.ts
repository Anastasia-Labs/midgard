/** Where our own view shows a staged step's operands staged, as our plan
 * reads it (one-step-staging.ts `lookup`): of the live staging outputs at
 * their confirmation depth, those a resolve move of ours read first, and,
 * past the PrepareSelected move, the staging whose evidence that move
 * committed, from our own view when it is no longer live; and a wait on the
 * L1 source when it cannot say how deep a staging output stands. */
import {
  midgardCekStagingAddress,
  midgardCekStagingResult,
  planMidgardCekStaging,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  CML,
  generateEmulatorAccount,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { validationTraceStaging } from "../src/validation-trace-dispute/one-step-staging.js";
import type {
  ValidationTraceResolveReader,
  ValidationTraceStagingOutput,
} from "../src/validation-trace-dispute/one-step-staging-journal.js";
import type { ValidationTraceOwnViewOf } from "../src/validation-trace-dispute/own-view.js";
import { FraudProofL1UnavailableError } from "../src/workflow/l1-source.js";

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

/** One staging of every run, from made-up inputs `seed` on: its staging
 * outputs as our own view shows them, in run order. */
const stagingOf = (seed: number) => {
  const stagings: ValidationTraceStagingOutput[] = RUNS.map(
    ({ datumCbor }, run) => {
      const tx = transaction({
        seed: seed + run,
        address: STAGING,
        datum: datumCbor,
      });
      return {
        run: `${datumCbor.slice(0, 8)}${run.toString()}`,
        txHash: tx.hash,
        outRef: `${tx.hash}#0`,
        datumCbor,
        thread: undefined,
        threadUnit: undefined,
      };
    },
  );
  return { outRefs: stagings.map(({ outRef }) => outRef), stagings };
};

/** A confirmed resolve move of ours reading `reads`. */
const readerOf = (
  reads: readonly string[],
  seed: number,
): ValidationTraceResolveReader => {
  const tx = transaction({ seed, reads });
  return {
    txHash: tx.hash,
    signed: tx.cbor,
    reads: new Set(reads),
    outputs: [],
  };
};

const first = stagingOf(0x100);
const second = stagingOf(0x200);
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

/** Our lookup over an own view showing both stagings, each read by a
 * resolve move of ours (the second the later), with `live` the outputs the
 * ledger still holds, the provider showing each staging transaction 100
 * blocks deep, or, when `uncounted`, confirmed at a depth it cannot say. */
const lookupOver = ({
  live = [...first.outRefs, ...second.outRefs],
  uncounted = false,
}: {
  readonly live?: readonly string[];
  readonly uncounted?: boolean;
} = {}) => {
  const lucid = {
    config: () => ({
      network: "Custom",
      provider: {
        getTransactionStatus: async () => ({
          status: "confirmed",
          confirmation: { confirmations: uncounted ? undefined : 100 },
        }),
      },
    }),
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
  const ownView: ValidationTraceOwnViewOf = () =>
    ({
      stagings: async () => ({
        stagings: [...first.stagings, ...second.stagings],
        readers: [readerOf(first.outRefs, 1), readerOf(second.outRefs, 2)],
      }),
      inFlight: async () => [],
    }) as never;
  const staging = validationTraceStaging({
    lucid: lucid as never,
    signer: { paymentKeyHash: KEY, address: ours.address } as never,
    deployment: {} as never,
    caseHistory: {} as never,
    signedRecovery: {} as never,
    materialOf: () => undefined,
    ownView,
  });
  return staging.lookup({} as never);
};

describe("where our own view shows a staged step's operands staged", () => {
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

  it("names, past the PrepareSelected move, the staging its evidence names from our own view once an output of it is no longer live, and no other", async () => {
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
    await expect(
      lookupOver({ uncounted: true })(OPERANDS),
    ).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
  });
});
