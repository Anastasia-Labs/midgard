/** Production reclaim builder against real list/retention scripts; native fixture
 * hub and reclaim owner. No finalized-frontier or retirement claim is made. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  scriptFromNative,
  type UTxO,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, expect, it } from "vitest";

import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  index,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];

it("reclaims both retained event kinds only through their exact published authorization script", async () => {
  const h = await setupHistoryPair({ blueprint, records });
  const history = h.applied.map(
    (applied, i): SDK.EventHistoryContracts => ({
      recipe: h.recipes[i]!,
      list: {
        ...SDK.makeAuthenticatedValidator(
          "Custom",
          applied.validator.script,
          applied.validator.script,
        ),
        ...SDK.makeWithdrawalValidator(applied.validator.script),
      },
      retention: SDK.makeSpendingValidator(
        "Custom",
        applied.retention.validator.script,
      ),
      retirement: SDK.makeWithdrawalValidator(
        applied.retirement.validator.script,
      ),
    }),
  );
  // These fields reproduce the helper's explicitly native hub authority. The
  // production builder still resolves the actual hub and compares its full datum.
  const fixtureIssuer = {
    policyId: h.hubPolicy,
    spendingScriptAddress: h.hubAddress,
  };
  const contracts = {
    hubOracle: fixtureIssuer,
    registeredOperators: fixtureIssuer,
    activeOperators: fixtureIssuer,
    retiredOperators: fixtureIssuer,
    scheduler: fixtureIssuer,
    stateQueue: fixtureIssuer,
    fraudProofCatalogue: fixtureIssuer,
    fraudProof: fixtureIssuer,
    txOrder: fixtureIssuer,
    settlement: fixtureIssuer,
    payout: fixtureIssuer,
    reserve: { ...fixtureIssuer, withdrawalScriptHash: h.hubPolicy },
    deposit: history[0]!.list,
    withdrawal: history[1]!.list,
    eventHistory: { deposit: history[0]!, withdrawal: history[1]! },
  } as unknown as SDK.MidgardValidators;
  const authorization = h.issuer;
  const alternate = scriptFromNative({
    type: "all",
    scripts: [{ type: "sig", keyHash: h.owner }],
  });
  expect(validatorToScriptHash(alternate)).not.toBe(
    validatorToScriptHash(authorization),
  );
  const payloads = historyPairPayloads(h);
  const retainedDatums = payloads.map((payload, i) => {
    if ("DepositPayload" in payload)
      payload.DepositPayload.event.info.l2_datum = "ab".repeat(1000);
    else
      payload.WithdrawalPayload.event.info.body.l1_datum = {
        InlineDatum: { data: "ab".repeat(1000) },
      };
    const plan = SDK.prepareEventHistoryPayload(
      payload,
      { ScriptCredential: [h.hubPolicy] },
      h.recipes[i]!,
    );
    if (plan.kind !== "External")
      throw new Error("Expected real external retained payload");
    return plan.datumCbor;
  });
  let publication = h.lucid
    .newTx()
    .collectFrom(await h.funding())
    .register.Stake(validatorToRewardAddress("Custom", authorization))
    .pay.ToAddressWithData(
      h.wallet.address,
      undefined,
      { lovelace: 3_000_000n },
      authorization,
    )
    .pay.ToAddressWithData(
      h.wallet.address,
      undefined,
      { lovelace: 3_000_000n },
      alternate,
    );
  for (let i = 0; i < 2; i++)
    publication = publication.pay.ToContract(
      h.applied[i]!.retention.address,
      { kind: "inline", value: retainedDatums[i]! },
      { lovelace: 10_000_000n },
    );
  const published = await h.submit(
    "publish-owner-references-and-both-retained-payloads",
    await publication.complete({ coinSelection: false, localUPLCEval: true }),
  );
  const [authReference, wrongReference, depositData, withdrawalData] =
    await h.lucid.utxosByOutRef(
      [0, 1, 2, 3].map((outputIndex) => ({ txHash: published, outputIndex })),
    );
  expect(authReference?.scriptRef).toEqual(authorization);
  expect(wrongReference?.scriptRef).toEqual(alternate);
  const retained = [depositData!, withdrawalData!];
  for (const [i, kind] of (["Deposit", "Withdrawal"] as const).entries()) {
    const config: SDK.ReclaimEventHistoryDataConfig = {
      kind,
      retainedInput: retained[i]!,
      hubOracleRefInput: h.hub,
      scriptAuthorization: {
        script: authorization,
        redeemer: Data.void(),
        referenceInput: authReference!,
      },
    };
    for (const [label, scriptAuthorization, expected] of [
      [
        "wrong-owner-script",
        {
          script: alternate,
          redeemer: Data.void(),
          referenceInput: wrongReference!,
        },
        "exact retained script credential",
      ],
      [
        "wrong-owner-reference",
        {
          script: authorization,
          redeemer: Data.void(),
          referenceInput: wrongReference!,
        },
        "Wrong reclamation authorization reference script",
      ],
    ] as const) {
      const result = await Effect.runPromise(
        Effect.either(
          SDK.buildReclaimEventHistoryDataTxProgram(h.lucid, contracts, {
            ...config,
            scriptAuthorization,
          }),
        ),
      );
      expect(result._tag).toBe("Left");
      if (result._tag !== "Left")
        throw new Error("Mismatched authorization unexpectedly built");
      expect(String(result.left.cause)).toContain(expected);
      expect(await h.lucid.utxosByOutRef([retained[i]!])).toHaveLength(1);
      records.push({ label, kind, refusal: String(result.left.cause) });
    }
    const absence = await SDK.fetchEventHistoryWitness(
      h.lucid,
      SDK.eventHistoryDeploymentFromContracts(history[i]!),
      {
        transactionId: h.eventNonces[i]!.txHash,
        outputIndex: BigInt(h.eventNonces[i]!.outputIndex),
      },
    );
    expect(absence.kind).toBe("Absent");
    const built = await Effect.runPromise(
      SDK.buildReclaimEventHistoryDataTxProgram(h.lucid, contracts, config),
    );
    const body = built.tx.toTransaction().body();
    const references = body.reference_inputs()!;
    const actualReferences: UTxO[] = await h.lucid.utxosByOutRef(
      Array.from({ length: references.len() }, (_, j) => ({
        txHash: references.get(j).transaction_id().to_hex(),
        outputIndex: Number(references.get(j).index()),
      })),
    );
    expect(built.layout.absenceReferenceIndex).toBe(
      index(actualReferences, absence.anchor.utxo),
    );
    expect(built.layout.hubReferenceIndex).toBe(index(actualReferences, h.hub));
    expect(actualReferences).toContainEqual(authReference);
    const withdrawals = body.withdrawals()!;
    expect(withdrawals.len()).toBe(1);
    expect(withdrawals.get(withdrawals.keys().get(0))).toBe(0n);
    expect(withdrawals.keys().get(0).to_address().to_bech32()).toBe(
      validatorToRewardAddress("Custom", authorization),
    );
    expect(
      built.tx.toTransaction().witness_set().native_scripts()?.len() ?? 0,
    ).toBe(0);
    await h
      .submit(`reclaim-${kind}-with-published-owner-reference`, built.tx)
      .catch((cause: unknown) => {
        records.push({
          label: "reclaim-submission-failure",
          kind,
          unsignedTransactionCbor: built.tx.toCBOR(),
          cause: inspect(cause, { depth: 8 }),
        });
        throw new Error(
          `Reclaim ${kind} submission: ${inspect(cause, { depth: 8 })}`,
        );
      });
    expect(await h.lucid.utxosByOutRef([retained[i]!])).toHaveLength(0);
    expect(
      await h.lucid.utxosByOutRef([absence.anchor.utxo, authReference!]),
    ).toHaveLength(2);
  }
});

afterAll(() => {
  expect(readFileSync(realBlueprintPath).equals(blueprintBytes)).toBe(true);
  const directory = process.env.MIDGARD_EVENT_HISTORY_EVIDENCE_DIR;
  if (directory === undefined) return;
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, "history-reclaim-builders.json"),
    JSON.stringify(
      {
        scope:
          "Production reclaim builder with real initialized authenticated lists and retention scripts; native fixture hub and reclaim owner; never-admitted retained payloads, no frontier establishment",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        records,
      },
      (_, value) => (typeof value === "bigint" ? value.toString() : value),
      2,
    ),
  );
});
