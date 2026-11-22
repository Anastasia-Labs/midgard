/** The validation-trace dispute's own view (own-view.ts) read from the
 * chain: one capture of the challenged block's dispute per view
 * (own-view.chain-capture.ts), and our proof-materials view. Init needs our
 * signature and Open is ours alone, so a thread or pending claim whose datum
 * names our key is ours; the case an Open of ours started is ours whoever
 * verified it. Our resolve moves are the moves of our cases' threads out of
 * a one-step holder that our key signed, and what they read at our staging
 * address is our staging. Every answer is of the next view: the ledger
 * with our node's mempool applied (own-view.chain-capture.ts). */
import { createHash } from "node:crypto";

import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk/da";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { selectOwnFraudProofRecord } from "../fraud-proof-record-selection.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofNextView } from "../workflow/next-state.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { rawToUtxo } from "../workflow/raw-l1-family-derivation.state-queue-topology.js";
import type { FraudProofRawL1Utxo } from "../workflow/raw-l1-snapshot.js";
import {
  validationTraceDatumDigest,
  type ValidationTraceOutputClaim,
} from "./actions.js";
import {
  validationTraceArtifactOf,
  type ValidationTraceDisputeArtifact,
} from "./artifact.js";
import type { ValidationTraceCaseHistory } from "./case-history.js";
import { validationTraceFraudProofUnit } from "./close.js";
import { validationTraceOneStepHolderAt } from "./one-step-holders.js";
import type {
  ValidationTraceResolveReader,
  ValidationTraceStagingOutput,
  ValidationTraceStagings,
} from "./one-step-staging-types.js";
import {
  ownStepOutputs,
  type ValidationTraceDisputeCapture,
  validationTraceDisputeCapture,
  type ValidationTraceHistoryTransaction,
} from "./own-view.chain-capture.js";
import {
  sameDatum,
  signedBy,
  type ValidationTraceReferenced,
  validationTraceReferenced,
} from "./own-view.chain-reads.js";
import type {
  ValidationTraceOwnReceipt,
  ValidationTraceOwnView,
  ValidationTraceOwnViewOf,
} from "./own-view.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const digestOf = (datumCbor: string): string =>
  createHash("sha256").update(Buffer.from(datumCbor, "hex")).digest("hex");

/** The out-ref of `utxo`. */
const label = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The inline datum of `raw`, in the canonical encoding the capture holds
 * outputs in (raw-l1-snapshot.admit-fraud-proof-raw-l1-snapshot.ts). */
const datumOf = (raw: FraudProofRawL1Utxo): string | undefined => {
  const { datum } = rawToUtxo(raw);
  return typeof datum === "string" ? datum : undefined;
};

/** A resolve move of ours, with the exact outputs it read. */
type Reader = ValidationTraceResolveReader &
  Readonly<{
    referenced: readonly ValidationTraceReferenced[];
    /** The thread output it spent, and that thread's token. */
    spent: string;
    unit: string;
  }>;

/** The claim on the live output `raw` of ours, in `role`. */
const claimOf = (
  role: ValidationTraceOutputClaim["role"],
  raw: FraudProofRawL1Utxo,
  unit: string,
): ValidationTraceOutputClaim => {
  const utxo = rawToUtxo(raw);
  return Object.freeze({
    role,
    outRef: raw.outRef,
    address: utxo.address,
    datumCborSha256: validationTraceDatumDigest(utxo.datum!),
    unit,
  });
};

/** Throws unless the claim our datum `datum` commits to is `artifact`'s:
 * the trace recomputed for the workflow must be the one we committed. */
const sameClaim = (
  artifact: ValidationTraceDisputeArtifact,
  committed: { readonly challenger_root: string; readonly step_count: bigint },
  where: string,
) => {
  if (
    committed.challenger_root !== artifact.claim.challengerRoot ||
    committed.step_count !== artifact.claim.stepCount
  )
    throw new Error(
      `validation-trace ${where} commits to a trace other than the one recomputed for block ${artifact.headerHash}`,
    );
};

/**
 * The own view read from the chain. Each view captures the dispute once, on
 * its first read, and reads every answer from that capture; the units the
 * captures find are kept per header for the process, and so are our resolve
 * moves decoded from the current capture, which a long resolution confirms
 * hundreds of, with what each read: every hop of a long staged step reads
 * every staging output of the step, each hundreds of kilobytes, so each is
 * decoded once.
 */
export const validationTraceChainOwnView = ({
  lucid,
  signer,
  deployment,
  caseHistory,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly caseHistory: ValidationTraceCaseHistory;
}): ValidationTraceOwnViewOf => {
  const prover = signer.paymentKeyHash;
  const captureOf = validationTraceDisputeCapture({
    caseHistory,
    deployment,
    named: new Map(),
  });
  const decoded = new Map<string, Map<string, Reader | null>>();
  const read = new Map<string, Map<string, ValidationTraceReferenced>>();
  const stagingAddress = signer.proofMaterialsAddress;

  /** Our resolve moves in `capture`, oldest first. */
  const readersOf = ({
    snapshot,
    history,
    threads,
    outputsOf,
  }: ValidationTraceDisputeCapture): readonly Reader[] => {
    const known = decoded.get(snapshot.headerHash) ?? new Map();
    const knownRead = read.get(snapshot.headerHash) ?? new Map();
    const current = new Map<string, Reader | null>();
    const currentRead = new Map<string, ValidationTraceReferenced>();
    const referenced = (raw: FraudProofRawL1Utxo) => {
      const prior = currentRead.get(raw.outRef) ?? knownRead.get(raw.outRef);
      const found =
        prior?.raw.outputCbor === raw.outputCbor
          ? prior
          : validationTraceReferenced(raw, stagingAddress);
      currentRead.set(raw.outRef, found);
      return found;
    };
    const readers: Reader[] = [];
    const seen = new Set<string>();
    for (const { unit } of threads)
      for (const transaction of history(unit)) {
        if (seen.has(transaction.txHash)) continue;
        seen.add(transaction.txHash);
        // A pending transaction is read again each capture and kept for
        // none: what it spends and reads resolves fully only once the
        // ledger holds it. A kept one stands deeper at each capture.
        const kept = transaction.pending
          ? undefined
          : known.get(transaction.txHash);
        const reader =
          kept === undefined
            ? readerOf(transaction, unit, outputsOf, referenced)
            : kept === null
              ? null
              : Object.freeze({
                  ...kept,
                  confirmationDepth: transaction.confirmationDepth,
                });
        if (!transaction.pending) current.set(transaction.txHash, reader);
        if (reader === null) continue;
        readers.push(reader);
        for (const entry of reader.referenced)
          if (!currentRead.has(entry.raw.outRef))
            currentRead.set(entry.raw.outRef, entry);
      }
    decoded.set(snapshot.headerHash, current);
    read.set(snapshot.headerHash, currentRead);
    return readers;
  };
  /** `transaction` as a resolve move of ours of the thread `unit`, or null
   * for any other move. */
  const readerOf = (
    transaction: ValidationTraceHistoryTransaction,
    unit: string,
    outputsOf: ValidationTraceDisputeCapture["outputsOf"],
    referenced: (raw: FraudProofRawL1Utxo) => ValidationTraceReferenced,
  ): Reader | null => {
    const spent = transaction.resolvedInputs.find(
      (raw) => (rawToUtxo(raw).assets[unit] ?? 0n) === 1n,
    );
    const holder =
      spent === undefined
        ? undefined
        : validationTraceOneStepHolderAt(deployment, rawToUtxo(spent).address)
            ?.holder;
    if (
      spent === undefined ||
      holder === undefined ||
      holder === "award" ||
      !signedBy(transaction, prover)
    )
      return null;
    const outputs = outputsOf(transaction).map(({ output }) => ({
      address: output.address,
      assets: output.assets,
    }));
    if (
      !outputs.some(
        ({ address, assets }) =>
          assets[unit] === 1n &&
          validationTraceOneStepHolderAt(deployment, address) !== undefined,
      )
    )
      return null;
    return Object.freeze({
      txHash: transaction.txHash,
      confirmationDepth: transaction.confirmationDepth,
      reads: new Set(transaction.referenceInputs),
      outputs,
      referenced: transaction.resolvedReferenceInputs.map(referenced),
      spent: spent.outRef,
      unit,
    });
  };

  const viewOf = (
    context: Parameters<ValidationTraceOwnViewOf>[0],
  ): ValidationTraceOwnView => {
    const artifact = () => validationTraceArtifactOf(context);
    let captured: Promise<ValidationTraceDisputeCapture> | undefined;
    const capture = () => (captured ??= captureOf(artifact(), prover));
    const scope = (view: FraudProofNextView, address: string) =>
      view.scopes.find((candidate) => candidate.address === address)?.utxos ??
      [];
    /** The newest of `outputs` still live at `address`. */
    const newestLive = async (
      outputs: readonly { readonly outRef: string }[],
      address: string,
    ) => {
      const found = await capture();
      const live = scope(found.view, address);
      for (const { outRef } of [...outputs].reverse()) {
        const raw = live.find((candidate) => candidate.outRef === outRef);
        if (raw !== undefined) return { raw, threadUnit: found.threadUnit };
      }
      return undefined;
    };
    let stepsRead: ReturnType<typeof ownStepOutputs> | undefined;
    const own = async () => {
      const { history, outputsOf, threadUnit } = await capture();
      return (stepsRead ??= ownStepOutputs({
        history,
        outputsOf,
        deployment,
        threadUnit,
        prover,
      }));
    };
    let readersRead: Promise<readonly Reader[]> | undefined;
    const readers = () => (readersRead ??= capture().then(readersOf));
    const stagings = async () => {
      const found = await readers();
      const bound = new Map<string, ValidationTraceStagingOutput>();
      for (const reader of found)
        for (const { raw, ours, run: datumCbor } of reader.referenced) {
          if (bound.has(raw.outRef) || !ours || datumCbor === undefined)
            continue;
          bound.set(
            raw.outRef,
            Object.freeze({
              run: digestOf(datumCbor),
              txHash: raw.outRef.split("#")[0]!,
              outRef: raw.outRef,
              datumCbor,
              thread: reader.spent,
              threadUnit: reader.unit,
            }),
          );
        }
      return { readers: found, bound: [...bound.values()] };
    };
    return {
      artifact,
      thread: async () => {
        const live = await newestLive(
          (await own()).opener,
          deployment.chain.opener.spendingScriptAddress,
        );
        return live === undefined
          ? undefined
          : claimOf("thread", live.raw, live.threadUnit);
      },
      replacedThread: async () => (await own()).opener.at(-1)?.outRef,
      pendingClaim: async () => {
        const live = await newestLive(
          (await own()).source,
          deployment.chain.source.spendingScriptAddress,
        );
        if (live === undefined) return undefined;
        const { data } = Data.from(
          datumOf(live.raw)!,
          SDK.PendingValidationClaimDatum,
        );
        if (data !== null) sameClaim(artifact(), data, "pending claim");
        return claimOf("opener", live.raw, live.threadUnit);
      },
      cases: async () =>
        [...(await own()).source].reverse().map(({ outRef }) => outRef),
      caseAt: async (caseUnit) => {
        const { view } = await capture();
        const raw = scope(
          view,
          deployment.chain.case.spendingScriptAddress,
        ).find(
          (candidate) => (rawToUtxo(candidate).assets[caseUnit] ?? 0n) === 1n,
        );
        if (raw === undefined) return undefined;
        const utxo = rawToUtxo(raw);
        if (typeof utxo.datum === "string")
          sameClaim(artifact(), Data.from(utxo.datum, SDK.Case), "case");
        return utxo;
      },
      record: async () => {
        const { view } = await capture();
        const unit = validationTraceFraudProofUnit(
          deployment,
          artifact().headerHash,
        );
        const found = selectOwnFraudProofRecord({
          records: scope(
            view,
            deployment.fraudProof.spendingScriptAddress,
          ).filter((raw) => (rawToUtxo(raw).assets[unit] ?? 0n) === 1n),
          outRefOf: ({ outRef }) => outRef,
          datumOf,
          prover,
        });
        return found === undefined ? undefined : claimOf("proof", found, unit);
      },
      receipts: async () => {
        const { history, threads, outputsOf } = await capture();
        const receipts: (ValidationTraceOwnReceipt & { rank: bigint })[] = [];
        for (const { receiptUnit } of threads) {
          const transactions = history(receiptUnit);
          const consumed = new Set(
            transactions.flatMap(({ resolvedInputs }) =>
              resolvedInputs.map(({ outRef }) => outRef),
            ),
          );
          for (const { outRef, output, transaction } of transactions.flatMap(
            outputsOf,
          ))
            if (output.assets[receiptUnit] === 1n && !consumed.has(outRef))
              receipts.push({
                outRef,
                utxo: { ...parseOutRefLabel(outRef), ...output },
                rank: transaction.rank,
              });
        }
        return receipts
          .sort((a, b) => (a.rank < b.rank ? -1 : a.rank > b.rank ? 1 : 0))
          .map(({ outRef, utxo }) => ({ outRef, utxo }));
      },
      caseThreads: async () =>
        (await capture()).threads.map(({ unit }) => unit),
      stagings: async (): Promise<ValidationTraceStagings> => {
        const { readers, bound } = await stagings();
        const { view, holder } = proofMaterialsAccess({
          signer,
          lucid,
          holder: workflowProofMaterialsHolder(context),
        });
        return {
          stagings: bound,
          readers,
          // Every copy with those bytes, so one the provider does not yet
          // show by out-ref hides none of the others.
          byContent: async (datumCbor) => {
            const copiesOf = async (count: number) =>
              await view.find(Array<string>(count).fill(datumCbor), holder);
            let copies = await copiesOf(2);
            while (copies.at(-1) !== undefined)
              copies = await copiesOf(copies.length * 2);
            return copies.flatMap((utxo) =>
              utxo === undefined ||
              bound.some(({ outRef }) => outRef === label(utxo))
                ? []
                : [
                    Object.freeze({
                      run: digestOf(datumCbor),
                      txHash: utxo.txHash,
                      outRef: label(utxo),
                      datumCbor,
                      thread: undefined,
                      threadUnit: undefined,
                    }),
                  ],
            );
          },
        };
      },
      signedStagings: async () =>
        (await stagings()).bound.map(({ outRef, datumCbor }) => ({
          outRef,
          datumCbor,
        })),
      resolveReads: readers,
      // What our resolve moves read at our proof-materials address that
      // stages no run, while it is live: in the bytes its output holds,
      // which a spend back is checked against (carried-field.ts).
      publications: async () => {
        const seen = new Map<string, string>();
        for (const reader of await readers())
          for (const { raw, ours, datum, run } of reader.referenced)
            if (ours && datum !== undefined && run === undefined)
              seen.set(raw.outRef, datum);
        const live = [];
        for (const [outRef, datumCbor] of seen) {
          const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(outRef)]);
          if (
            utxo !== undefined &&
            typeof utxo.datum === "string" &&
            sameDatum(utxo.datum, datumCbor)
          )
            live.push({ outRef, datumCbor: utxo.datum });
        }
        return live;
      },
    };
  };
  // One view per context: every read of one observation or one build
  // shares its capture.
  const views = new WeakMap<object, ValidationTraceOwnView>();
  return (context) => {
    const known = views.get(context);
    if (known !== undefined) return known;
    const view = viewOf(context);
    views.set(context, view);
    return view;
  };
};
