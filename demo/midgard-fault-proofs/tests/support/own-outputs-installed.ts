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
 * Rows so far: script-decode staging and script-decode run here, field
 * publication and field certificate in `own-field-carriage-installed.ts`.
 * The other carriage roles join as their sites move onto `own-outputs`.
 */
import { startScriptDecodeWalk } from "@al-ft/midgard-validation";
import {
  CML,
  coreToTxOutput,
  Data,
  type Emulator,
  getAddressDetails,
  type LucidEvolution,
  mintingPolicyToId,
  scriptFromNative,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { plutusItemScript } from "../../src/script-decode/item-record.js";
import { stageScriptDecodeBytes } from "../../src/script-decode/stage.js";
import {
  bindScriptDecodeRun,
  scriptDecodeStagingDatums,
} from "../../src/script-decode/submit.js";
import { scriptDecodeDeploymentOf } from "./script-decode-emulator.js";
import { honestScript } from "./script-decode-plutus-max-shape.js";

/**
 * A Plutus V3 script of over 2 KB, carried as a reference script by a shape
 * plant: a transaction that read the plant would pay its reference-script
 * fee (15 lovelace a byte), well past the fee spread the cost check allows.
 */
const REFERENCE_SCRIPT = honestScript(2_048).toString("hex");

/** Outputs per transaction of the mass plant. */
const MASS_BATCH = 25;

/** Distinct tokens in the token-bearing coin plant. */
const TOKEN_BUNDLE = 40;

/** The carriage roles a row covers, and where their outputs sit. */
export type OwnOutputRole = Readonly<{
  role: "script_decode_staging" | "script_decode_run";
  /** The address the role's outputs are paid to. */
  address: string;
  /** The datums the application's own outputs of the role hold. */
  datums: readonly string[];
}>;

/** The script-decode roles of one Plutus item, over `emulator`'s deployment. */
export const scriptDecodeRoles = ({
  emulator,
  proverAddress,
  item,
}: {
  readonly emulator: Emulator;
  readonly proverAddress: string;
  readonly item: Uint8Array;
}) => {
  const script = plutusItemScript(item);
  if (script === undefined) throw new Error("the item carries no script");
  const deployment = scriptDecodeDeploymentOf(emulator);
  return {
    script,
    staging: {
      role: "script_decode_staging",
      address: proverAddress,
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

const submitAs = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  build: (lucid: LucidEvolution) => ReturnType<LucidEvolution["newTx"]>,
): Promise<string> =>
  await watch.asOtherParty(async () => {
    lucid.clearUTxOOverride();
    const unsigned = await build(lucid).complete();
    const signed = await unsigned.sign.withWallet().complete();
    return await signed.submit();
  });

/** A native-script token the other party can mint. */
const otherPartyToken = (address: string) => {
  const keyHash = getAddressDetails(address).paymentCredential!.hash;
  const policy = scriptFromNative({ type: "sig", keyHash });
  return { policy, unit: `${mintingPolicyToId(policy)}cafe` };
};

/**
 * Outputs holding `role`'s datums in other shapes: with a token, with a
 * reference script, and as a datum hash.
 */
export const plantShapes = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
) => {
  const { policy, unit } = otherPartyToken(await lucid.wallet().address());
  await submitAs(watch, lucid, (other) => {
    let tx = other
      .newTx()
      .mintAssets({ [unit]: BigInt(role.datums.length) })
      .attach.MintingPolicy(policy);
    for (const datum of role.datums)
      tx = tx.pay
        .ToAddressWithData(
          role.address,
          { kind: "inline", value: datum },
          { [unit]: 1n },
        )
        .pay.ToAddressWithData(
          role.address,
          { kind: "inline", value: datum },
          {},
          { type: "PlutusV3", script: REFERENCE_SCRIPT },
        )
        .pay.ToAddressWithData(
          role.address,
          { kind: "asHash", value: datum },
          {},
        );
    return tx;
  });
};

/**
 * One coin at `address` with no datum and no script, more ada than the
 * largest coin there, and a bundle of `TOKEN_BUNDLE` tokens: the coin a
 * wallet's coin selection would reach for first were tokens no bar.
 */
export const plantTokenCoin = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  address: string,
) => {
  const { policy } = otherPartyToken(await lucid.wallet().address());
  const policyId = mintingPolicyToId(policy);
  const bundle: Record<string, bigint> = {};
  for (let index = 0; index < TOKEN_BUNDLE; index += 1)
    bundle[`${policyId}${index.toString(16).padStart(4, "0")}`] = 1n;
  const largest = (await lucid.utxosAt(address)).reduce(
    (max, { assets }) => (assets.lovelace! > max ? assets.lovelace! : max),
    0n,
  );
  await submitAs(watch, lucid, (other) =>
    other
      .newTx()
      .mintAssets(bundle)
      .attach.MintingPolicy(policy)
      .pay.ToAddress(address, {
        lovelace: largest + 1_000_000_000n,
        ...bundle,
      }),
  );
};

/** Exact copies of `role`'s outputs: the same datums, ada only, inline. */
export const plantCopies = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
) => {
  await submitAs(watch, lucid, (other) => {
    let tx = other.newTx();
    for (const datum of role.datums)
      tx = tx.pay.ToAddressWithData(
        role.address,
        { kind: "inline", value: datum },
        {},
      );
    return tx;
  });
};

/**
 * An exact copy of `datum` at `address` whose transaction id sorts before
 * `below`: submitted ahead of the transaction it copies, into the same block.
 */
export const plantCopyBefore = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  { address, datum, below }: { address: string; datum: string; below: string },
) => {
  lucid.clearUTxOOverride();
  for (let attempt = 0; attempt < 4_096; attempt += 1) {
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(
        address,
        { kind: "inline", value: datum },
        { lovelace: 20_000_000n + BigInt(attempt) },
      )
      .complete();
    const signed = await unsigned.sign.withWallet().complete();
    if (signed.toHash() < below) {
      return await watch.asOtherParty(async () => await signed.submit());
    }
  }
  throw new Error("no copy sorting first was found");
};

/**
 * `count` datum-bearing outputs at `role`'s address, in transactions of
 * `MASS_BATCH`, a block each.
 */
export const plantMass = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  role: OwnOutputRole,
  { emulator, count }: { emulator: Emulator; count: number },
) => {
  for (let start = 0; start < count; start += MASS_BATCH) {
    await submitAs(watch, lucid, (other) => {
      let tx = other.newTx();
      for (
        let index = start;
        index < Math.min(count, start + MASS_BATCH);
        index += 1
      )
        tx = tx.pay.ToAddressWithData(
          role.address,
          { kind: "inline", value: Data.to(BigInt(index)) },
          {},
        );
      return tx;
    });
    emulator.awaitBlock();
  }
};

/**
 * The other party's own record of the same script, begun: it stages the
 * bytes at its own address and Binds a run of them.
 */
export const plantForeignRun = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  {
    emulator,
    script,
  }: {
    emulator: Emulator;
    script: NonNullable<ReturnType<typeof plutusItemScript>>;
  },
): Promise<UTxO> =>
  await watch.asOtherParty(async () => {
    const staging = await stageScriptDecodeBytes({
      lucid,
      scriptBytes: script.scriptBytes,
    });
    return await bindScriptDecodeRun({
      lucid,
      deployment: scriptDecodeDeploymentOf(emulator),
      language: script.language,
      scriptHashHex: script.scriptHashHex,
      initialRun: startScriptDecodeWalk(script.language, script.scriptBytes)
        .run,
      staging,
    });
  });

/**
 * Reads of the chain that do not yet show `outRef` while `hidden()` holds:
 * a provider lagging the chain, or a rollback that has not yet re-included
 * the transaction. The ledger itself is unchanged.
 */
export const lagReadsOf = (
  emulator: Emulator,
  outRef: () => string | undefined,
) => {
  let hiddenReads = 0;
  const visible = (utxos: UTxO[]) => {
    const hidden = outRef();
    if (hidden === undefined) return utxos;
    const shown = utxos.filter(
      ({ txHash, outputIndex }) =>
        `${txHash}#${outputIndex.toString()}` !== hidden,
    );
    if (shown.length < utxos.length) hiddenReads += 1;
    return shown;
  };
  const byOutRef = emulator.getUtxosByOutRef.bind(emulator);
  const atAddress = emulator.getUtxos.bind(emulator);
  const spies = [
    vi
      .spyOn(emulator, "getUtxosByOutRef")
      .mockImplementation(async (outRefs) => visible(await byOutRef(outRefs))),
    vi
      .spyOn(emulator, "getUtxos")
      .mockImplementation(async (address) => visible(await atAddress(address))),
  ];
  return {
    /** Reads that would have shown the out-ref and did not. */
    hiddenReads: () => hiddenReads,
    restore: () => spies.forEach((spy) => spy.mockRestore()),
  };
};

/**
 * The application's wall clock (`new Date()`, which its journal stamps
 * entries with) reading the emulator, as `Date.now` already does under the
 * installed application clock: journal times, retry intervals and the
 * provider grace then all run on the one chain clock.
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

/**
 * Takes `utxo` (at `lucid`'s wallet) off the chain, its ada going to
 * `to`: the chain as it is once the transaction that created it is rolled
 * back for good. Recorded as the world acting, not the application.
 */
export const removeOutput = async (
  watch: LedgerWatch,
  lucid: LucidEvolution,
  utxo: UTxO,
  to: string,
) => {
  await watch.asOtherParty(async () => {
    lucid.clearUTxOOverride();
    const unsigned = await lucid
      .newTx()
      .collectFrom([utxo])
      .complete({
        presetWalletInputs: [utxo],
        coinSelection: false,
        changeAddress: to,
      });
    await (await unsigned.sign.withWallet().complete()).submit();
    lucid.clearUTxOOverride();
  });
};
