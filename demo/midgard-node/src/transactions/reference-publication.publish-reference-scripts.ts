import { setTimeout as pause } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  IntentJournal,
  type IntentJournalService,
} from "../services/intent-journal.js";
import {
  sendPublication,
  submitPublicationFunding,
} from "./reference-publication.journal.js";
import {
  CONSOLIDATION_INPUTS,
  key,
  type Lane,
  MAX_OUTSTANDING_BYTES,
  type Publication,
  referencePublicationFundingRequired,
  referencePublicationLaneCount,
  type ReferencePublicationOptions,
} from "./reference-publication.reference-publication-funding-required.js";
import { signOverWalletView } from "./utils.wallet-view.js";
import { isPlainAdaOnlyUtxo } from "./wallet-hygiene.js";

/** Called only by the existing publication entry point. No records outlive this invocation. */
export const publishReferenceScripts = async ({
  lucid,
  address,
  targets,
  authPolicy,
  reserved,
  options,
  minAuthPolicyRemainingMs,
  signal,
  journal,
}: {
  lucid: LucidEvolution;
  address: string;
  targets: readonly SDK.ReferenceScriptTarget[];
  authPolicy: SDK.ReferenceScriptAuthMintingPolicy;
  reserved: ReadonlySet<string>;
  options: ReferencePublicationOptions;
  minAuthPolicyRemainingMs: number;
  signal?: AbortSignal;
  /** Every publication and funding tx is journaled before its first send. */
  journal: IntentJournalService;
}): Promise<readonly SDK.ReferenceScriptResolved[]> => {
  const startedAt = Date.now();
  const metrics = {
    transactions: 0,
    signedBytes: 0,
    submissionAttempts: 0,
    submissionDurationMs: 0,
    peakOutstandingCount: 0,
    peakOutstandingBytes: 0,
  };
  if (lucid.config().provider === undefined)
    throw new Error("Reference publication requires a provider");
  const walletAddress = await lucid.wallet().address();
  const wait = options.wait ?? (() => pause(1_000));
  let deadline = Date.now() + 30 * 60_000;
  const madeProgress = () => {
    deadline = Date.now() + 30 * 60_000;
  };
  const waitForProgress = async () => {
    signal?.throwIfAborted();
    if (Date.now() >= deadline)
      throw new Error(
        "Reference publication reconciliation timed out; restart must resolve previous validity windows",
      );
    await wait();
  };
  const units = new Map(
    targets.map((target) => [
      target.name,
      SDK.referenceScriptAuthUnit(authPolicy.policyId, target.name),
    ]),
  );
  const targetByUnit = new Map(
    targets.map((target) => [units.get(target.name)!, target]),
  );
  // Cache immutable script-content hashes, never confirmation or UTxO evidence.
  const scriptHashes = new Map<string, string>();
  const scriptHash = (script: Script): string => {
    const content = `${script.type}:${script.script}`;
    let hash = scriptHashes.get(content);
    if (hash === undefined) {
      hash = validatorToScriptHash(script);
      scriptHashes.set(content, hash);
    }
    return hash;
  };
  const matches = (utxo: UTxO, target: SDK.ReferenceScriptTarget): boolean =>
    utxo.address === address &&
    utxo.assets[units.get(target.name)!] === 1n &&
    utxo.scriptRef != null &&
    utxo.scriptRef.type === target.script.type &&
    scriptHash(utxo.scriptRef) === scriptHash(target.script);
  const readReferences = async (): Promise<ReadonlyMap<string, UTxO>> => {
    const outputs = await lucid.utxosAt(address);
    const references = new Map<string, UTxO>();
    for (const output of outputs) {
      for (const unit of Object.keys(output.assets)) {
        const target = targetByUnit.get(unit);
        if (target === undefined) continue;
        if (!matches(output, target))
          throw new Error(`Malformed authenticated reference: ${target.name}`);
        if (references.has(target.name))
          throw new Error(`Duplicate authenticated reference: ${target.name}`);
        references.set(target.name, output);
      }
    }
    return references;
  };
  const missing = (
    roster: readonly SDK.ReferenceScriptTarget[],
    references: ReadonlyMap<string, UTxO>,
  ) => roster.filter((target) => !references.has(target.name));
  const resolveRoster = (
    references: ReadonlyMap<string, UTxO>,
  ): readonly SDK.ReferenceScriptResolved[] =>
    targets.map((target) => {
      const utxo = references.get(target.name);
      if (utxo === undefined)
        throw new Error(
          `Missing authenticated reference script: ${target.name}`,
        );
      return { name: target.name, utxo };
    });
  await options.synchronize();
  let canonical = await readReferences();
  if (missing(targets, canonical).length === 0) return resolveRoster(canonical);

  // Lost memory cannot prove whether an earlier submission was accepted. Every
  // invocation crosses the enforced upper validity bound before allocating funds.
  const restartBound = lucid.unixTimeToSlot(
    lucid.slotToUnixTime(lucid.currentSlot()) +
      SDK.REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS,
  );
  await Effect.runPromise(
    Effect.logInfo(
      `Reference publication restart reconciliation: mode=${options.mode},wait_until_slot=${restartBound}`,
    ),
  );
  while ((await options.synchronize()) < restartBound) await waitForProgress();
  canonical = await readReferences();
  const remaining = missing(targets, canonical);
  SDK.assertReferenceScriptRawBodiesFitL1Envelope(remaining);
  if (remaining.length > 0)
    SDK.assertReferenceScriptAuthMinimumRemaining({
      policy: authPolicy,
      nowMs: lucid.slotToUnixTime(lucid.currentSlot()),
      minRemainingMs: minAuthPolicyRemainingMs,
      scopeName: "reference publication",
      targetNames: remaining.map((t) => t.name),
    });

  const laneCount = referencePublicationLaneCount(options.mode);
  const depth = options.mode === "chained" ? 3 : 1;
  const batches: SDK.ReferenceScriptTarget[][] = [];
  for (let i = 0; i < remaining.length; i += 4)
    batches.push(remaining.slice(i, i + 4));
  // The wallet view (§8.5): own facts no live intent holds, plus the change
  // live own intents predict.
  const funding = (
    await Effect.runPromise(journal.walletView(lucid, walletAddress))
  ).utxos.filter(
    (utxo) => isPlainAdaOnlyUtxo(utxo) && !reserved.has(key(utxo)),
  );
  if (batches.length === 0) return resolveRoster(canonical);
  const funds = remaining.map((target) => scriptHash(target.script));
  if (funding.length === 0)
    throw new Error(
      "No confirmed plain wallet funding for reference publication",
    );
  const fundingCheckpoints = [[...funding]];
  // A lane spends all its assigned roots. Compact fragmented funding first so
  // neither the lane split nor a single-script publication inherits hundreds
  // of inputs. Each plain transfer is confirmed before its output is reused.
  // Finish at one common ancestor before splitting: a rollback of preparation
  // must not leave one lane confirmed while the other loses its funding roots.
  const consolidationRequired = funding.length > laneCount;
  while (consolidationRequired && funding.length > 1) {
    signal?.throwIfAborted();
    const inputs = funding.slice(0, CONSOLIDATION_INPUTS);
    const unsigned = await lucid
      .newTx()
      .collectFrom(inputs)
      .validTo(
        lucid.slotToUnixTime(lucid.currentSlot()) +
          SDK.REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS,
      )
      .complete({
        localUPLCEval: true,
        coinSelection: false,
        presetWalletInputs: inputs,
      });
    const outputs = unsigned.toTransaction().body().outputs();
    if (outputs.len() !== 1)
      throw new Error(
        "Funding consolidation must produce one plain wallet output",
      );
    const output = coreToTxOutput(outputs.get(0));
    const hash = await Effect.runPromise(
      submitPublicationFunding(journal, lucid, unsigned, "consolidate", {
        funds,
        confirmationTimeoutMs: 30 * 60_000,
        confirmationRetries: 0,
        requiredOutputIndexes: [0],
      }),
      { signal },
    );
    await options.synchronize();
    const [confirmed] = await lucid.utxosByOutRef([
      { txHash: hash, outputIndex: 0 },
    ]);
    if (
      confirmed === undefined ||
      !isPlainAdaOnlyUtxo(confirmed) ||
      confirmed.address !== walletAddress ||
      confirmed.assets.lovelace !== output.assets.lovelace
    )
      throw new Error(
        "Canonical funding consolidation output differs from the signed transaction",
      );
    funding.splice(0, inputs.length);
    // Chain preparation too, so a fork can only restore a recorded prefix.
    funding.unshift(confirmed);
    fundingCheckpoints.push([...funding]);
    madeProgress();
  }
  const groups: UTxO[][] = Array.from({ length: laneCount }, () => []);
  const balances = Array<bigint>(laneCount).fill(0n);
  for (const utxo of SDK.orderReferenceScriptFundingUtxos(funding)) {
    const lane = laneCount === 1 || balances[0]! <= balances[1]! ? 0 : 1;
    groups[lane]!.push(utxo);
    balances[lane]! += utxo.assets.lovelace;
  }
  const requirements = groups.map((_, index) =>
    referencePublicationFundingRequired(
      lucid,
      address,
      batches.filter((_, i) => i % laneCount === index).flat(),
      authPolicy,
    ),
  );
  let splitWasConfirmed = false;
  // Reuse disjoint confirmed roots when sufficient. Otherwise confirm one split.
  if (
    laneCount === 2 &&
    groups.some((_, i) => balances[i]! < requirements[i]!)
  ) {
    const balance = balances[0]! + balances[1]!;
    const available =
      balance - SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
    if (available < requirements[0]! + requirements[1]!)
      throw new Error(
        `Insufficient plain wallet funding for two publication lanes: available=${available.toString()},required=${(requirements[0]! + requirements[1]!).toString()}`,
      );
    const surplus = available - requirements[0]! - requirements[1]!;
    const amounts = [
      requirements[0]! + surplus / 2n,
      requirements[1]! + surplus - surplus / 2n,
    ];
    const unsigned = await lucid
      .newTx()
      .collectFrom([...funding])
      .pay.ToAddress(walletAddress, { lovelace: amounts[0]! })
      .pay.ToAddress(walletAddress, { lovelace: amounts[1]! })
      .validTo(
        lucid.slotToUnixTime(lucid.currentSlot()) +
          SDK.REFERENCE_SCRIPT_PUBLICATION_VALIDITY_MS,
      )
      .complete({
        localUPLCEval: true,
        coinSelection: false,
        presetWalletInputs: [...funding],
      });
    const splitOutputs = unsigned.toTransaction().body().outputs();
    const requiredOutputIndexes: number[] = [];
    for (const amount of amounts) {
      const index = Array.from(
        { length: splitOutputs.len() },
        (_, i) => i,
      ).find((i) => {
        const output = coreToTxOutput(splitOutputs.get(i));
        return (
          !requiredOutputIndexes.includes(i) &&
          output.address === walletAddress &&
          output.assets.lovelace === amount &&
          output.scriptRef == null &&
          output.datum == null &&
          output.datumHash == null &&
          Object.keys(output.assets).length === 1
        );
      });
      if (index === undefined)
        throw new Error("Funding split omitted an exact lane output");
      requiredOutputIndexes.push(index);
    }
    signal?.throwIfAborted();
    const hash = await Effect.runPromise(
      submitPublicationFunding(journal, lucid, unsigned, "split", {
        funds,
        confirmationTimeoutMs: 30 * 60_000,
        confirmationRetries: 0,
        requiredOutputIndexes,
      }),
      { signal },
    );
    const refs = requiredOutputIndexes.map((outputIndex) => ({
      txHash: hash,
      outputIndex,
    }));
    while (true) {
      await options.synchronize();
      const outputs = await lucid.utxosByOutRef(refs);
      if (
        outputs.length === 2 &&
        outputs.every(
          (utxo) => utxo.address === walletAddress && isPlainAdaOnlyUtxo(utxo),
        )
      ) {
        groups[0] = outputs.filter(
          (output) =>
            output.outputIndex === requiredOutputIndexes[0] &&
            output.assets.lovelace === amounts[0],
        );
        groups[1] = outputs.filter(
          (output) =>
            output.outputIndex === requiredOutputIndexes[1] &&
            output.assets.lovelace === amounts[1],
        );
        if (groups.some((group) => group.length !== 1))
          throw new Error(
            "Canonical split outputs differ from the signed transaction",
          );
        splitWasConfirmed = true;
        break;
      }
      await waitForProgress();
    }
  }
  const lanes: Lane[] = Array.from({ length: laneCount }, (_, index) => {
    const roots = groups[index]!;
    return {
      funding: roots,
      roots,
      queue: batches.filter((_, i) => i % laneCount === index),
      records: [],
      recovering: false,
    };
  });
  const outstanding = () =>
    lanes.flatMap((lane) => lane.records.filter((record) => !record.confirmed));
  const withinOutstandingByteBudget = () =>
    outstanding().reduce((sum, record) => sum + record.cbor.length / 2, 0) <=
    MAX_OUTSTANDING_BYTES;
  const send = sendPublication(journal, lucid);
  const submit = async (record: Publication) => {
    signal?.throwIfAborted();
    const submittedAt = Date.now();
    metrics.submissionAttempts += 1;
    try {
      // Transport errors and submit rejections are both inconclusive about an
      // earlier accepted copy. Retry identical bytes; never replace while valid.
      record.lastSubmission = await send(record);
      record.accepted = record.lastSubmission.outcome === "accepted";
    } finally {
      metrics.submissionDurationMs += Date.now() - submittedAt;
    }
  };
  let needsObservation = options.mode === "serial";
  publicationLoop: while (true) {
    signal?.throwIfAborted();
    if (needsObservation) {
      const slot = await options.synchronize();
      canonical = await readReferences();
      for (const lane of lanes) {
        for (const record of lane.records) {
          const confirmed = record.targets.every((target) => {
            const utxo = canonical.get(target.name);
            return (
              utxo?.txHash === record.hash &&
              utxo.outputIndex === record.referenceIndexes.get(target.name)
            );
          });
          if (!record.confirmed && confirmed) madeProgress();
          if (record.confirmed && !confirmed) lane.recovering = true;
          record.confirmed = confirmed;
          if (!confirmed && slot >= record.expiresAtSlot)
            lane.recovering = true;
        }
      }
      for (const lane of lanes) {
        const unresolved = lane.records.filter((record) => !record.confirmed);
        if (
          lane.recovering &&
          unresolved.every((record) => slot >= record.expiresAtSlot)
        ) {
          const candidates = [
            ...lane.roots,
            ...lane.records.flatMap((record) => record.outputs),
          ].filter(isPlainAdaOnlyUtxo);
          const live = await lucid.utxosByOutRef(candidates);
          const exact = live.filter(
            (utxo) =>
              candidates.some(
                (candidate) =>
                  key(candidate) === key(utxo) &&
                  candidate.address === utxo.address &&
                  candidate.assets.lovelace === utxo.assets.lovelace,
              ) && isPlainAdaOnlyUtxo(utxo),
          );
          if (exact.length === 0) {
            const records = lanes.flatMap((candidate) => candidate.records);
            if (
              (splitWasConfirmed || fundingCheckpoints.length > 1) &&
              records.every((record) => !record.confirmed)
            ) {
              if (records.some((record) => slot < record.expiresAtSlot)) {
                await waitForProgress();
                needsObservation = true;
                continue publicationLoop;
              }
              const ancestors = [
                ...new Map(
                  fundingCheckpoints.flat().map((input) => [key(input), input]),
                ).values(),
              ];
              const restored = await lucid.utxosByOutRef(ancestors);
              if (
                fundingCheckpoints.some((checkpoint) =>
                  checkpoint.every((input) =>
                    restored.some(
                      (utxo) =>
                        key(utxo) === key(input) &&
                        utxo.address === input.address &&
                        utxo.assets.lovelace === input.assets.lovelace &&
                        isPlainAdaOnlyUtxo(utxo),
                    ),
                  ),
                )
              ) {
                // Funding preparation rolled back. Both lane suffixes are
                // expired and an exact pre-consolidation or pre-split funding
                // set is canonical again. Restart also drains preparation TTLs.
                return await publishReferenceScripts({
                  lucid,
                  address,
                  targets,
                  authPolicy,
                  reserved,
                  options,
                  minAuthPolicyRemainingMs,
                  signal,
                  journal,
                });
              }
            }
            throw new Error(
              "Cannot reconcile a usable publication lane output",
            );
          }
          const retry = missing(
            lane.records.flatMap((record) => record.targets),
            canonical,
          );
          for (let i = retry.length; i > 0; i -= 4)
            lane.queue.unshift(retry.slice(Math.max(0, i - 4), i));
          lane.records.splice(
            0,
            lane.records.length,
            ...lane.records.filter((record) => record.confirmed),
          );
          lane.funding = exact;
          lane.recovering = false;
        } else {
          for (const record of unresolved) {
            if (
              unresolved.length <= depth &&
              withinOutstandingByteBudget() &&
              slot < record.expiresAtSlot &&
              (!record.accepted || lane.recovering)
            ) {
              await submit(record);
              if (!record.accepted) break;
            }
          }
        }
      }
    }
    if (
      missing(targets, canonical).length === 0 &&
      outstanding().length === 0
    ) {
      // Re-read the entire roster after synchronization. A cached confirmation
      // or signed output is never completion evidence.
      await options.synchronize();
      const verified = await readReferences();
      if (missing(targets, verified).length !== 0) continue;
      await Effect.runPromise(
        Effect.logInfo(
          `Reference publication complete: ${JSON.stringify({ ...metrics, mode: options.mode, wallTimeMs: Date.now() - startedAt })}`,
        ),
      );
      return resolveRoster(verified);
    }
    if (
      lanes.every((lane) => lane.queue.length === 0) &&
      outstanding().length === 0
    ) {
      throw new Error(
        "Previously published references disappeared; restart requires canonical reconciliation before rebuilding",
      );
    }
    let progressed = false;
    // A rollback can withdraw several generations of confirmation at once.
    // Wait for canonical resolution/expiry before resubmitting or extending
    // that backlog; old confirmed bytes must not escape the submission caps.
    if (!withinOutstandingByteBudget()) {
      needsObservation = true;
      await waitForProgress();
      continue;
    }
    for (const lane of lanes) {
      const pending = lane.records.filter((record) => !record.confirmed);
      if (
        lane.recovering ||
        pending.length >= depth ||
        pending.some((record) => !record.accepted)
      )
        continue;
      const batch = lane.queue[0];
      if (batch === undefined) continue;
      const needed = missing(batch, canonical);
      if (needed.length === 0) {
        lane.queue.shift();
        progressed = true;
        continue;
      }
      SDK.assertReferenceScriptAuthMinimumRemaining({
        policy: authPolicy,
        nowMs: lucid.slotToUnixTime(lucid.currentSlot()),
        minRemainingMs: minAuthPolicyRemainingMs,
        scopeName: "reference publication",
        targetNames: needed.map((t) => t.name),
      });
      let record: Publication;
      try {
        const { tx, layout } = await Effect.runPromise(
          SDK.completeReferenceScriptPublicationTxProgram({
            lucid,
            selectedFundingInputs: lane.funding,
            walletAddress,
            referenceScriptsAddress: address,
            missingTargets: needed,
            authPolicy,
          }),
        );
        // Signed over the wallet view: a lane's funding may be the
        // predicted change of its previous, still live, publication.
        const signed = await (
          await Effect.runPromise(
            signOverWalletView(lucid, tx).pipe(
              Effect.provideService(IntentJournal, journal),
            ),
          )
        ).complete();
        const cbor = signed.toCBOR();
        const hash = signed.toHash();
        const body = CML.Transaction.from_cbor_hex(cbor).body();
        const expiry = body.ttl();
        if (expiry === undefined)
          throw new Error("Publication is missing a validity upper bound");
        const outputs = Array.from(
          { length: body.outputs().len() },
          (_, outputIndex): UTxO => {
            const output = coreToTxOutput(body.outputs().get(outputIndex));
            return {
              txHash: hash,
              outputIndex,
              address: output.address,
              assets: output.assets,
              datum: output.datum ?? undefined,
              datumHash: output.datumHash ?? undefined,
              scriptRef: output.scriptRef ?? undefined,
            };
          },
        );
        if (
          !needed.every((target) =>
            outputs.some((utxo) => matches(utxo, target)),
          )
        )
          throw new Error(
            "Signed publication is missing an authenticated reference output",
          );
        record = {
          hash,
          cbor,
          signed,
          inputs: lane.funding,
          outputs,
          targets: needed,
          expiresAtSlot: Number(expiry),
          referenceIndexes: new Map(
            [...layout.localReferenceOutputs].map(([name, output]) => [
              name,
              output.outputIndex,
            ]),
          ),
          accepted: false,
          confirmed: false,
        };
      } catch (cause) {
        if (
          needed.length > 1 &&
          /Max transaction size of \d+ exceeded/.test(String(cause))
        ) {
          const middle = Math.ceil(needed.length / 2);
          lane.queue.splice(
            0,
            1,
            needed.slice(0, middle),
            needed.slice(middle),
          );
          progressed = true;
          continue;
        }
        throw cause;
      }
      if (record.cbor.length / 2 > MAX_OUTSTANDING_BYTES)
        throw new Error(
          "Signed publication exceeds the outstanding byte budget",
        );
      if (
        outstanding().reduce((sum, tx) => sum + tx.cbor.length / 2, 0) +
          record.cbor.length / 2 >
        MAX_OUTSTANDING_BYTES
      )
        continue;
      lane.queue.shift();
      madeProgress();
      lane.records.push(record);
      metrics.transactions += 1;
      metrics.signedBytes += record.cbor.length / 2;
      metrics.peakOutstandingCount = Math.max(
        metrics.peakOutstandingCount,
        outstanding().length,
      );
      metrics.peakOutstandingBytes = Math.max(
        metrics.peakOutstandingBytes,
        outstanding().reduce(
          (sum, pendingRecord) => sum + pendingRecord.cbor.length / 2,
          0,
        ),
      );
      lane.funding = record.outputs.filter(
        (utxo) => utxo.address === walletAddress && isPlainAdaOnlyUtxo(utxo),
      );
      await submit(record);
      await Effect.runPromise(
        Effect.logInfo(
          `Reference publication submission: txHash=${record.hash},outcome=${record.lastSubmission?.outcome},bytes=${record.cbor.length / 2},targets=[${record.targets.map((target) => target.name).join(",")}]`,
        ),
      );
      progressed = true;
    }
    needsObservation = !progressed || options.mode === "serial";
    if (!progressed) await waitForProgress();
  }
};
