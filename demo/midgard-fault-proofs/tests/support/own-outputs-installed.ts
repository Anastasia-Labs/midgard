/**
 * The installed application's own carriage among outputs other parties place
 * at the same addresses. A table row per carriage role names where that
 * role's outputs sit and what they hold; the plants put other parties'
 * outputs there (other shapes with the same content, exact copies confirmed
 * and pending, a mass of outputs, a run of their own), and the ledger watch
 * records every transaction the ledger takes, so a test can check that the
 * application's own transactions spend and reference only the application's
 * own outputs and cost what they cost with nothing planted.
 *
 * Rows here: script-decode staging and run, and the validation-trace
 * dispute's pending claim, case and bond; field publication and certificate
 * rows are in `own-field-carriage-installed.ts`. Other roles join later.
 */
import {
  CML,
  coreToTxOutput,
  type Emulator,
  type TxOutput,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { plutusItemScript } from "../../src/script-decode/item-record.js";
import { scriptDecodeStagingDatums } from "../../src/script-decode/submit.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
export {
  plantCopies,
  plantCopyBefore,
  plantForeignRun,
  plantMass,
  plantShapes,
  plantTokenCoin,
  removeOutput,
} from "./own-outputs-installed.plants.js";

/** The carriage roles a row covers, and where their outputs sit. */
export type OwnOutputRole = Readonly<{
  role:
    | "script_decode_staging"
    | "script_decode_run"
    | "validation_trace_opener"
    | "validation_trace_case"
    | "validation_trace_bond";
  /** The address the role's outputs are paid to. */
  address: string;
  /** The datums the application's own outputs of the role hold. */
  datums: readonly string[];
}>;

/** A Plutus item's script-decode roles, staged at `materialsAddress`. */
export const scriptDecodeRoles = ({
  emulator,
  materialsAddress,
  item,
}: {
  readonly emulator: Emulator;
  readonly materialsAddress: string;
  readonly item: Uint8Array;
}) => {
  const script = plutusItemScript(item);
  if (script === undefined) throw new Error("the item carries no script");
  const deployment = scriptDecodeDeploymentOf(emulator);
  return {
    script,
    staging: {
      role: "script_decode_staging",
      address: materialsAddress,
      datums: scriptDecodeStagingDatums(script.scriptBytes),
    } satisfies OwnOutputRole,
    run: {
      role: "script_decode_run",
      address: deployment.validator.spendingScriptAddress,
      datums: [],
    } satisfies OwnOutputRole,
  } as const;
};

/** One transaction the ledger took. */
export type LedgerTransaction = Readonly<{
  txHash: string;
  fee: bigint;
  inputs: readonly string[];
  referenceInputs: readonly string[];
  outputs: readonly TxOutput[];
  withdraws: boolean;
  /** True when another party placed it, not the application. */
  planted: boolean;
  /** The signed transaction as submitted. */
  cbor: string;
}>;

const outRefList = (
  list: CML.TransactionInputList | undefined,
): readonly string[] => {
  const outRefs: string[] = [];
  for (let index = 0; list !== undefined && index < list.len(); index += 1) {
    const input = list.get(index);
    outRefs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return outRefs;
};

const decode = (cbor: string, planted: boolean): LedgerTransaction => {
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  const outputs: TxOutput[] = [];
  for (let index = 0; index < body.outputs().len(); index += 1)
    outputs.push(coreToTxOutput(body.outputs().get(index)));
  const withdrawals = body.withdrawals();
  return Object.freeze({
    txHash: CML.hash_transaction(body).to_hex(),
    fee: body.fee(),
    inputs: outRefList(body.inputs()),
    referenceInputs: outRefList(body.reference_inputs()),
    outputs,
    withdraws: withdrawals !== undefined && withdrawals.len() > 0,
    planted,
    cbor,
  });
};

/** What the ledger does with one of the application's transactions. */
export type LedgerVerdict = "accept" | "withhold";

/**
 * Watches every transaction submitted to `emulator`. `decide` sees each of
 * the application's transactions before the ledger does: "withhold" loses
 * it in flight (the submitter is told its hash, the ledger never sees it).
 * `planting` runs other parties' submissions, which are recorded as planted.
 */
export const watchLedger = (
  emulator: Emulator,
  decide: (
    transaction: LedgerTransaction,
  ) => Promise<LedgerVerdict> | LedgerVerdict = () => "accept",
) => {
  const accepted: LedgerTransaction[] = [];
  const withheld: LedgerTransaction[] = [];
  let planting = 0;
  const submit = emulator.submitTx.bind(emulator);
  const spy = vi
    .spyOn(emulator, "submitTx")
    .mockImplementation(async (cbor: string) => {
      const transaction = decode(cbor, planting > 0);
      if (!transaction.planted && (await decide(transaction)) === "withhold") {
        withheld.push(transaction);
        return transaction.txHash;
      }
      const txHash = await submit(cbor);
      accepted.push(transaction);
      return txHash;
    });
  return {
    accepted,
    withheld,
    /** The application's own transactions the ledger took, in order. */
    ours: () => accepted.filter(({ planted }) => !planted),
    /** Out-refs of every output another party placed. */
    plantedOutRefs: () =>
      new Set(
        accepted
          .filter(({ planted }) => planted)
          .flatMap(({ txHash, outputs }) =>
            outputs.map((_, index) => `${txHash}#${index.toString()}`),
          ),
      ),
    /** Runs `act` as another party: what it submits is planted. */
    asOtherParty: async <T>(act: () => Promise<T>): Promise<T> => {
      planting += 1;
      try {
        return await act();
      } finally {
        planting -= 1;
      }
    },
    restore: () => spy.mockRestore(),
  };
};

export type LedgerWatch = ReturnType<typeof watchLedger>;

/** An output another party placed that a transaction of the application
 * spent or referenced: which transaction of ours took it, how, and the
 * output as it was placed. */
export type TakenPlanted = Readonly<{
  outRef: string;
  by: string;
  as: "input" | "reference";
  shape: Readonly<{
    address: string;
    datum: string | null;
    datumHash: string | null;
    scriptRef: boolean;
    assets: Readonly<Record<string, string>>;
  }>;
}>;

/** Every transaction the ledger accepted, in order: whose it was, what it
 * spent and referenced, and where its outputs went. */
export const ledgerTimeline = (watch: LedgerWatch) =>
  watch.accepted.map(
    ({ txHash, planted, inputs, referenceInputs, outputs }) => ({
      txHash: txHash.slice(0, 12),
      planted,
      inputs: inputs.map((outRef) => outRef.slice(0, 12) + outRef.slice(64)),
      references: referenceInputs.map(
        (outRef) => outRef.slice(0, 12) + outRef.slice(64),
      ),
      outputs: outputs.map(({ address, datum, assets }) => ({
        address: address.slice(-12),
        datum: datum?.slice(0, 16) ?? null,
        tokens: Object.keys(assets).length - 1,
      })),
    }),
  );

/** Every output another party placed that the application's transactions
 * took, outside `excluded`, in the order they were taken. */
export const takenPlanted = (
  watch: LedgerWatch,
  excluded: ReadonlySet<string> = new Set(),
): readonly TakenPlanted[] => {
  const placed = new Map<string, TxOutput>();
  for (const { txHash, outputs, planted } of watch.accepted)
    if (planted)
      for (const [index, output] of outputs.entries())
        placed.set(`${txHash}#${index.toString()}`, output);
  return watch.ours().flatMap(({ txHash, inputs, referenceInputs }) =>
    [
      ...inputs.map((outRef) => ({ outRef, as: "input" as const })),
      ...referenceInputs.map((outRef) => ({
        outRef,
        as: "reference" as const,
      })),
    ].flatMap(({ outRef, as }) => {
      const output = placed.get(outRef);
      if (output === undefined || excluded.has(outRef)) return [];
      return [
        {
          outRef,
          by: txHash,
          as,
          shape: {
            address: output.address,
            datum: output.datum ?? null,
            datumHash: output.datumHash ?? null,
            scriptRef: output.scriptRef != null,
            assets: Object.fromEntries(
              Object.entries(output.assets).map(([unit, quantity]) => [
                unit,
                quantity.toString(),
              ]),
            ),
          },
        },
      ];
    }),
  );
};

/** The shape of an output another party placed that the application took. */
export type TakenShape = TakenPlanted["shape"];

/**
 * Whether a placed output has the strict shape of carriage: an inline datum,
 * no datum hash, no reference script, and no token beyond those `tokens`
 * admits (none by default).
 */
export const strictShape = (
  shape: TakenShape,
  tokens: (unit: string, quantity: string) => boolean = () => false,
): boolean =>
  shape.datum !== null &&
  shape.datumHash === null &&
  !shape.scriptRef &&
  Object.entries(shape.assets).every(
    ([unit, quantity]) => unit === "lovelace" || tokens(unit, quantity),
  );

/**
 * The placed outputs the application took, split into the exact copies of
 * carriage it adopted (`strict` names their shape) and the rest.
 */
export const adoptedAndRejected = (
  taken: readonly TakenPlanted[],
  strict: (shape: TakenShape) => boolean = (shape) => strictShape(shape),
) => ({
  adopted: taken.filter(({ shape }) => strict(shape)),
  rejected: taken.filter(({ shape }) => !strict(shape)),
});

/** Every output the ledger accepted, ours and other parties', by out-ref. */
export const ledgerOutputs = (
  watch: LedgerWatch,
): ReadonlyMap<string, TxOutput> =>
  new Map(
    watch.accepted.flatMap(({ txHash, outputs }) =>
      outputs.map(
        (output, index) => [`${txHash}#${index.toString()}`, output] as const,
      ),
    ),
  );

/** The application's transaction that stages a chunk of `role`, and the out-ref of that chunk. */
export const stagedChunkOf = (
  transaction: LedgerTransaction,
  role: OwnOutputRole,
): string | undefined => {
  const index = transaction.outputs.findIndex(
    (output) =>
      output.address === role.address &&
      typeof output.datum === "string" &&
      role.datums.includes(output.datum),
  );
  return index < 0 ? undefined : `${transaction.txHash}#${index.toString()}`;
};

/** A transaction's kind, as the cost comparison counts it. */
export const kindOf = (
  transaction: LedgerTransaction,
  staging: OwnOutputRole,
  run: OwnOutputRole,
): string => {
  if (transaction.outputs.some((output) => output.address === run.address))
    return transaction.withdraws ? "walk" : "bind-or-finish";
  if (stagedChunkOf(transaction, staging) !== undefined) return "stage";
  return "step";
};

/**
 * The application's wall clock (`new Date()`) reading the emulator, as
 * `Date.now` already does under the installed application clock: retry
 * intervals and the provider grace then all run on the one chain clock.
 */
export const emulatorWallClock = () => {
  const RealDate = globalThis.Date;
  class EmulatorDate extends RealDate {
    constructor(...args: unknown[]) {
      if (args.length === 0) super(RealDate.now());
      else super(...(args as [string | number]));
    }
  }
  globalThis.Date = EmulatorDate as DateConstructor;
  return {
    restore: () => {
      globalThis.Date = RealDate;
    },
  };
};
