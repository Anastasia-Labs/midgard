import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  key,
  type OutRef,
  type PublicationChainBackend,
  PublicationJournal,
  type PublicationSchedule,
  type PublicationTransaction,
  publicationTransaction,
} from "./reference-publication-chain.publication-journal.js";
import { ReferencePublicationChain } from "./reference-publication-chain.reference-publication-chain.js";

export const publishReferenceChain = async (
  params: Readonly<{
    lucid: LucidEvolution;
    targets: readonly SDK.ReferenceScriptTarget[];
    authPolicy: SDK.ReferenceScriptAuthPolicy;
    journalPath: string;
    maxTargetsPerBatch: number;
    schedule: PublicationSchedule;
    publicationLimit: (role: string) => number;
    synchronize: () => Promise<number>;
    wait: () => Promise<void>;
    now: () => number;
    priorPublications?: readonly {
      role: string;
      signedCbor: string;
      outRef: OutRef;
    }[];
  }>,
) => {
  const { lucid, authPolicy, targets } = params;
  const walletAddress = await lucid.wallet().address();
  const journal = await PublicationJournal.open(params.journalPath);
  const startedAt = params.now();
  const startedAtWall = Date.now();
  let constructionDurationMs = 0;
  const backend: PublicationChainBackend = {
    submit: (tx) => lucid.config().provider!.submitTx(tx.signedCbor),
    wait: params.wait,
    observe: async (tx) => {
      const slot = await params.synchronize();
      const outputs = await lucid.utxosByOutRef(
        tx.roles.map(({ outputIndex }) => ({ txHash: tx.hash, outputIndex })),
      );
      const confirmed = tx.roles.every(({ role, outputIndex }) => {
        const output = outputs.find((utxo) => utxo.outputIndex === outputIndex);
        const expected = targets.find((target) => target.name === role);
        return (
          expected !== undefined &&
          output?.scriptRef != null &&
          validatorToScriptHash(output.scriptRef) ===
            validatorToScriptHash(expected.script) &&
          output.assets[
            SDK.referenceScriptAuthUnit(authPolicy.policyId, role)
          ] === 1n
        );
      });
      const transactionStatus = confirmed
        ? undefined
        : await lucid.transactionStatus(tx.hash);
      const rootInputs = tx.inputs.filter(
        ({ txHash }) =>
          !journal.records.has(txHash) ||
          journal.records.get(txHash)!.outcome === "confirmed",
      );
      const visibleInputs =
        confirmed || rootInputs.length === 0
          ? []
          : await lucid.utxosByOutRef(rootInputs);
      const conflictingInputs =
        confirmed || transactionStatus?.status !== "not_found"
          ? []
          : rootInputs.filter(
              (input) =>
                !visibleInputs.some((visible) => key(visible) === key(input)),
            );
      return { slot, confirmed, conflictingInputs };
    },
  };
  const scheduler = new ReferencePublicationChain(
    journal,
    backend,
    params.schedule,
  );
  try {
    if (journal.records.size === 0 && params.priorPublications?.length) {
      const imported = new Map<
        string,
        { signedCbor: string; roles: { role: string; outputIndex: number }[] }
      >();
      for (const receipt of params.priorPublications) {
        const entry = imported.get(receipt.outRef.txHash) ?? {
          signedCbor: receipt.signedCbor,
          roles: [],
        };
        if (
          entry.signedCbor !== receipt.signedCbor ||
          entry.roles.some(({ role }) => role === receipt.role)
        )
          throw new Error(
            "Retained publication receipts disagree or duplicate a role",
          );
        entry.roles.push({
          role: receipt.role,
          outputIndex: receipt.outRef.outputIndex,
        });
        imported.set(receipt.outRef.txHash, entry);
      }
      for (const [hash, entry] of imported) {
        const tx = publicationTransaction(
          entry.signedCbor,
          entry.roles,
          new Set(journal.records.keys()),
          params.now(),
        );
        if (tx.hash !== hash || !(await backend.observe(tx)).confirmed)
          throw new Error(
            "Retained publication must have matching signed bytes and canonical references before adoption",
          );
        await journal.prepare(tx);
        await journal.outcome(
          hash,
          "confirmed",
          "adopted retained canonical signed receipt",
        );
      }
    }
    // Journal identity is authenticated against the current policy and scripts,
    // including before replaying any recorded signed bytes.
    for (const { transaction: tx } of journal.records.values()) {
      for (const { role, outputIndex } of tx.roles) {
        const output = tx.outputs[outputIndex]?.utxo;
        const target = targets.find((candidate) => candidate.name === role);
        if (
          target === undefined ||
          output?.address !== walletAddress ||
          output.scriptRef == null ||
          validatorToScriptHash(output.scriptRef) !==
            validatorToScriptHash(target.script) ||
          output.assets[
            SDK.referenceScriptAuthUnit(authPolicy.policyId, role)
          ] !== 1n
        )
          throw new Error(
            "Publication journal differs from deployment identity",
          );
      }
    }
    await scheduler.resume();
    if (
      [...journal.records.values()].some(
        ({ outcome }) => outcome === "rejected",
      )
    )
      throw new Error(
        "Publication parent and descendants rejected and reconciled; replacements require proven mutually exclusive funding, or a new authority after expiry",
      );
    const assigned = new Set(
      [...journal.records.values()]
        .filter(({ outcome }) => outcome !== "rejected")
        .flatMap(({ transaction }) =>
          transaction.roles.map(({ role }) => role),
        ),
    );
    const confirmedWallet = await lucid.wallet().getUtxos();
    for (const target of targets) {
      if (
        !assigned.has(target.name) &&
        confirmedWallet.some(
          (utxo) =>
            (utxo.assets[
              SDK.referenceScriptAuthUnit(authPolicy.policyId, target.name)
            ] ?? 0n) !== 0n,
        )
      )
        throw new Error(
          `Reference ${target.name} exists without a signed journal record`,
        );
    }
    let remaining = targets.filter(({ name }) => !assigned.has(name));
    let funding: readonly UTxO[];
    if (
      remaining.length > 0 &&
      (await params.synchronize()) >= authPolicy.expiresAtSlot
    )
      throw new Error(
        "Publication authority expired with missing roles; invalid descendants are resolved and this identity cannot mint replacements",
      );
    const tail = [...journal.records.values()]
      .filter(({ outcome }) => outcome !== "rejected")
      .at(-1)?.transaction;
    const tailFunding = tail?.outputs[tail.fundingOutputIndex]?.utxo;
    const usableTail =
      tailFunding !== undefined &&
      (journal.records.get(tailFunding.txHash)!.outcome !== "confirmed" ||
        (await lucid.utxosByOutRef([tailFunding])).length === 1);
    if (tail !== undefined && usableTail)
      funding = [tail.outputs[tail.fundingOutputIndex]!.utxo];
    else {
      funding = SDK.selectReferenceScriptFundingUtxos(
        confirmedWallet,
        SDK.referenceScriptPublicationFundingTarget(targets.length) +
          BigInt(targets.length) * 10_000_000n,
      );
      if (funding.length === 0)
        throw new Error(
          "Insufficient plain confirmed funding for planned reference publication workload",
        );
    }
    while (remaining.length > 0) {
      const constructionStarted = Date.now();
      let count = 0;
      let estimatedBytes = 1024;
      for (const candidate of remaining.slice(0, params.maxTargetsPerBatch)) {
        const candidateBytes = candidate.script.script.length / 2 + 512;
        if (count > 0 && estimatedBytes + candidateBytes > 15_000) break;
        count += 1;
        estimatedBytes += candidateBytes;
      }
      let transaction: PublicationTransaction | undefined;
      while (count > 0) {
        const batch = remaining.slice(0, count);
        try {
          const { tx, layout } = await Effect.runPromise(
            SDK.completeReferenceScriptPublicationTxProgram({
              lucid,
              selectedFundingInputs: funding,
              walletAddress,
              referenceScriptsAddress: walletAddress,
              missingTargets: batch,
              authPolicy,
            }),
          );
          // Seed-wallet signing also resolves input owners. Give it the same
          // exact predecessor context used by completion, then release the view.
          lucid.overrideUTxOs([...funding]);
          const signed = await tx.sign
            .withWallet()
            .complete()
            .finally(() => lucid.clearUTxOOverride());
          if (
            batch.some(
              ({ name }) =>
                signed.toCBOR().length / 2 > params.publicationLimit(name),
            )
          ) {
            if (count === 1)
              throw new Error(
                `${batch[0]!.name} exceeds signed publication limit (${signed.toCBOR().length / 2} > ${params.publicationLimit(batch[0]!.name)} bytes)`,
              );
            count -= 1;
            continue;
          }
          transaction = publicationTransaction(
            signed.toCBOR(),
            batch.map(({ name }) => {
              const output = layout.localReferenceOutputs.get(name);
              if (output === undefined)
                throw new Error(`Missing publication output ${name}`);
              return { role: name, outputIndex: output.outputIndex };
            }),
            new Set(journal.records.keys()),
            params.now(),
          );
          break;
        } catch (cause) {
          // Lucid enforces maxTxSize before signing. Only a size error justifies
          // retrying a smaller batch; evaluation/funding failures remain fatal.
          if (
            count > 1 &&
            /maximum transaction size|max.*tx.*size|transaction.*too.*large|maximum.*size|MaxTxSize|MaxTransactionSize/i.test(
              String(cause),
            )
          ) {
            count -= 1;
            continue;
          }
          throw cause;
        }
      }
      if (transaction === undefined)
        throw new Error("Could not construct signed publication batch");
      constructionDurationMs += Date.now() - constructionStarted;
      await scheduler.enqueue(transaction);
      funding = [transaction.outputs[transaction.fundingOutputIndex]!.utxo];
      remaining = remaining.slice(count);
    }
    await scheduler.drain();
    if (scheduler.pending().length !== 0)
      throw new Error(
        "Reference publication ended with unresolved transactions",
      );
    return {
      transactions: [...journal.records.values()]
        .filter(({ outcome }) => outcome === "confirmed")
        .map(({ transaction }) => transaction),
      metrics: {
        ...scheduler.metrics,
        transactionCount: [...journal.records.values()].filter(
          ({ outcome }) => outcome === "confirmed",
        ).length,
        rejectedTransactions: [...journal.records.values()].filter(
          ({ outcome }) => outcome === "rejected",
        ).length,
        durationMs: Date.now() - startedAtWall,
        chainDurationMs: params.now() - startedAt,
        constructionDurationMs,
      },
    };
  } finally {
    await journal.close();
  }
};
