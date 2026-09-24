import { randomUUID } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  historyPairPayloads,
  setupHistoryPair,
} from "@al-ft/midgard-fault-proofs/test-support/history-pair";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";
import { expect, it, vi } from "vitest";

import * as Journal from "../src/database/eventHistorySubmissions.js";
import { submitDurableEventHistoryProgram } from "../src/transactions/event-history-submission.js";
import { provideDatabaseLayers } from "./utils.js";

/** Genuine history policies; the shared emulator fixture uses a native hub.
 * This combines the production node journal/driver with applied scripts, not
 * the deployed state-queue/frontier or live provider acceptance gates. */
it.each(["Deposit", "Withdrawal"] as const)(
  "recovers %s publication after a signing crash without preparing a new nonce",
  async (kind) => {
    const blueprint = SDK.parseFaultProofBlueprint(
      JSON.parse(
        readFileSync(
          process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
            new URL("../../../onchain/aiken/plutus.json", import.meta.url),
          "utf8",
        ),
      ),
    );
    const h = await setupHistoryPair({ blueprint, records: [] });
    const history = (index: number): SDK.EventHistoryContracts => {
      const applied = h.applied[index]!;
      return {
        recipe: h.recipes[index]!,
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
      };
    };
    const pair = { deposit: history(0), withdrawal: history(1) };
    const contracts: SDK.UserHistoryContracts = {
      hubOracle: {
        policyId: h.hubPolicy,
        mintingScript: h.issuer,
        mintingScriptCBOR: h.issuer.script,
        spendingScript: h.issuer,
        spendingScriptCBOR: h.issuer.script,
        spendingScriptHash: h.hubPolicy,
        spendingScriptAddress: h.hubAddress,
      },
      eventHistory: pair,
      deposit: pair.deposit.list,
      withdrawal: pair.withdrawal.list,
    };
    const index = kind === "Deposit" ? 0 : 1;
    const payload = historyPairPayloads(h)[index]!;
    if ("DepositPayload" in payload)
      payload.DepositPayload.event.info.l2_datum = "ab".repeat(600);
    else
      payload.WithdrawalPayload.refund_datum = {
        InlineDatum: { data: "ab".repeat(600) },
      };
    const request: SDK.EventHistorySubmissionRequest = {
      payload,
      nonce: h.eventNonces[index]!,
      assets: { lovelace: kind === "Deposit" ? 25_000_000n : 20_000_000n },
      structuralLovelace: kind === "Deposit" ? 5_000_000n : 0n,
      structuralRefundKey: h.owner,
      reclaimAuth: { PublicKeyCredential: [h.owner] },
    };
    const prepare = vi.fn(() => Effect.succeed({ request }));
    const submissionId = `emulator-${randomUUID()}`;
    const run = () =>
      Effect.runPromise(
        provideDatabaseLayers(
          submitDurableEventHistoryProgram({
            lucid: h.lucid,
            contracts,
            kind,
            submissionId,
            intentHash: "ab".repeat(32),
            nonceInput: request.nonce,
            scriptReference: h.scripts[index]!,
            prepare,
          }),
        ),
      );
    const now = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const signing = vi
      .spyOn(h.lucid.wallet(), "signTx")
      .mockRejectedValueOnce(new Error("simulated crash before signature"));
    try {
      await expect(run()).rejects.toThrow("requires reconciliation");
      const saved = await Effect.runPromise(
        provideDatabaseLayers(Journal.retrieve(submissionId)),
      );
      if (Option.isNone(saved))
        throw new Error("Crash lost its durable request");
      expect(saved.value.checkpoint.pending?.phase).toBe("Publication");
      expect(saved.value.request.nonce.txHash).toBe(request.nonce.txHash);
      const result = await run();
      expect(prepare).toHaveBeenCalledTimes(1);
      expect(result.checkpoint.publicationAttempt).toEqual(
        saved.value.checkpoint.pending,
      );
      expect(result.request.nonce).toEqual(request.nonce);
      expect(result.checkpoint.pending).toBeUndefined();
      expect(
        (await h.lucid.transactionStatus(result.admission.txHash)).status,
      ).toBe("confirmed");
      const resumed = await run();
      expect(resumed.admission).toEqual(result.admission);
      expect(prepare).toHaveBeenCalledTimes(1);
    } finally {
      signing.mockRestore();
      now.mockRestore();
    }
  },
  180_000,
);
