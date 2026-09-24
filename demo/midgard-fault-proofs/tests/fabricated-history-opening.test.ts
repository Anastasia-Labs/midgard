import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, datumToHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { fabricatedProofValidity } from "../src/fabricated-proof-validity.js";
import { deriveFabricatedDepositStep01Handoff } from "../src/submit-fabricated-deposit-step-01.js";
import {
  deriveFabricatedDepositStep03Handoff,
  parseSubmitFabricatedDepositAuthenticContent,
} from "../src/submit-fabricated-deposit-step-03.js";
import { deriveFabricatedWithdrawalStep01Handoff } from "../src/submit-fabricated-withdrawal-step-01.js";
import {
  deriveFabricatedWithdrawalStep03Handoff,
  parseSubmitFabricatedWithdrawalAuthenticContent,
} from "../src/submit-fabricated-withdrawal-step-03.js";
import {
  buildCountedRoot,
  keyValuePhasProof,
} from "../src/transition-trace/phas.js";
import { buildCanonicalBlockFixture } from "./helpers/canonical-block-evidence-fixture.js";

const owner = "12".repeat(28);
const policy = "34".repeat(28);
const id = { transactionId: "56".repeat(32), outputIndex: 2n };
const address = Effect.runSync(
  SDK.addressDataFromBech32(
    credentialToAddress("Custom", { type: "Key", hash: owner }),
  ),
);
const originalAssets: SDK.Value = new Map([
  ["", new Map([["", 20_000_000n]])],
  ["78".repeat(28), new Map([["ab", 3n]])],
]);
const payloadFor = (kind: "Deposit" | "Withdrawal"): SDK.EventHistoryPayload =>
  kind === "Deposit"
    ? {
        DepositPayload: {
          event: {
            id,
            info: {
              l2_address: address,
              l2_network_id: 0n,
              l2_datum: Data.from("182a"),
            },
          },
        },
      }
    : {
        WithdrawalPayload: {
          event: {
            id,
            info: {
              body: {
                l2_outref: id,
                l2_owner: owner,
                l2_value: originalAssets,
                l1_address: address,
                l1_datum: "NoDatum",
              },
              signature: ["aa".repeat(32), "bb".repeat(64)],
              validity: "WithdrawalIsValid",
            },
          },
          refund_address: address,
          refund_datum: "NoDatum",
        },
      };
const opening = (payload: SDK.EventHistoryPayload, assets = originalAssets) =>
  Data.to(
    { RetainedEventData: { payload, original_assets: assets } },
    SDK.FabricatedDepositAuthenticContentOpening,
  );

for (const kind of ["Deposit", "Withdrawal"] as const)
  describe(`${kind} production retained-opening handoff`, () => {
    const payload = payloadFor(kind);
    const authenticHash = Effect.runSync(
      "DepositPayload" in payload
        ? SDK.depositInfoCommitment(payload.DepositPayload.event.info)
        : SDK.withdrawalContentCommitment(payload.WithdrawalPayload.event.info),
    );
    const run = async ({
      inclusionTime = 150n,
      committedHash = authenticHash,
      supplied = opening(payload),
      absent = false,
      stateId = id,
      rawPayload,
    }: {
      inclusionTime?: bigint;
      committedHash?: string;
      supplied?: string | null;
      absent?: boolean;
      stateId?: SDK.OutputReference;
      rawPayload?: string;
    } = {}) => {
      const common = {
        state_queue_policy: policy,
        challenged_header_hash: "cc".repeat(28),
        header_start_time: 100n,
        header_end_time: 200n,
      };
      const commitment = SDK.eventHistoryCommitment(
        policy,
        kind,
        { event_id: id, inclusion_time: inclusionTime },
        payload,
        originalAssets,
      );
      if (rawPayload !== undefined)
        commitment.payload_hash = datumToHash(
          aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload),
        );
      return kind === "Deposit"
        ? deriveFabricatedDepositStep03Handoff({
            state: {
              ...common,
              committed_deposit_id: stateId,
              committed_deposit_info_hash: committedHash,
              verdict: absent
                ? "DepositIdentityAbsent"
                : { DepositEventObserved: { commitment } },
            },
            openingCbor: supplied ?? undefined,
          })
        : deriveFabricatedWithdrawalStep03Handoff({
            state: {
              ...common,
              committed_withdrawal_id: stateId,
              committed_withdrawal_content_hash: committedHash,
              verdict: absent
                ? "WithdrawalIdentityAbsent"
                : { WithdrawalEventObserved: { commitment } },
            },
            openingCbor: supplied ?? undefined,
          });
    };
    it("binds raw duplicate-pair content through the retained handoff", async () => {
      let raw = Data.to(payload, SDK.EventHistoryPayload);
      const datum = "a302c2410101d86682008002c2420001";
      raw = replacePlutusConstrFieldCbor(
        raw,
        kind === "Deposit" ? [0, 1, 2] : [0, 1, 0, 4],
        kind === "Deposit" ? `d8799f${datum}ff` : `d87b9f${datum}ff`,
      );
      const supplied = replacePlutusConstrFieldCbor(opening(payload), [0], raw);
      const info = plutusConstrFieldCbor(raw, [0, 1]);
      const rawHash = Effect.runSync(
        kind === "Deposit"
          ? SDK.depositInfoCommitmentCbor(info)
          : SDK.withdrawalContentCommitmentCbor(info),
      );
      const typedHash = Effect.runSync(
        kind === "Deposit"
          ? SDK.depositInfoCommitment(Data.from(info, SDK.DepositInfo))
          : SDK.withdrawalContentCommitment(
              Data.from(info, SDK.WithdrawalInfo),
            ),
      );
      expect(rawHash).not.toBe(typedHash);
      const result = await run({
        rawPayload: raw,
        supplied,
        committedHash: typedHash,
      });
      expect(result.openingCbor).toBe(supplied);
      expect(result.fault).toMatchObject({
        [`Mismatched${kind}Content`]: {
          [kind === "Deposit"
            ? "authentic_deposit_info_hash"
            : "authentic_withdrawal_content_hash"]: rawHash,
        },
      });
      await expect(
        run({ rawPayload: raw, supplied, committedHash: rawHash }),
      ).rejects.toThrow(/matches/);
      await expect(
        run({
          rawPayload: raw,
          supplied: Data.to(
            Data.from(supplied, SDK.FabricatedDepositAuthenticContentOpening),
            SDK.FabricatedDepositAuthenticContentOpening,
          ),
          committedHash: typedHash,
        }),
      ).rejects.toThrow(/commitment/);
      if (kind === "Withdrawal") {
        const relabelled = replacePlutusConstrFieldCbor(
          info,
          [2],
          Data.to("NonExistentWithdrawalUtxo", SDK.WithdrawalValidity),
        );
        expect(
          Effect.runSync(SDK.withdrawalContentCommitmentCbor(relabelled)),
        ).toBe(rawHash);
      }
    });
    it("classifies authenticated absence with no opening", async () => {
      expect((await run({ absent: true, supplied: null })).fault).toBe(
        `Nonexistent${kind}Identity`,
      );
      await expect(run({ absent: true })).rejects.toThrow(/absence/);
      await expect(run({ supplied: null })).rejects.toThrow(/requires/);
    });
    it("refuses honest content throughout the eligible interval, including its end", async () => {
      await expect(run()).rejects.toThrow(/matches/);
      await expect(run({ inclusionTime: 200n })).rejects.toThrow(/matches/);
    });
    it("classifies the start boundary and future events before testing content equality", async () => {
      for (const inclusionTime of [100n, 201n])
        for (const committedHash of [authenticHash, "00".repeat(32)])
          expect((await run({ inclusionTime, committedHash })).fault).toEqual({
            [`Ineligible${kind}Event`]: { event_inclusion_time: inclusionTime },
          });
    });
    it("classifies substituted eligible content with both commitments", async () => {
      const result = await run({ committedHash: "00".repeat(32) });
      const field = kind === "Deposit" ? "deposit_info" : "withdrawal_content";
      expect(result.fault).toEqual({
        [`Mismatched${kind}Content`]: {
          [`committed_${field}_hash`]: "00".repeat(32),
          [`authentic_${field}_hash`]: authenticHash,
          event_inclusion_time: 150n,
        },
      });
    });
    it("refuses changed original assets, wrong payload kind and a different thread identity", async () => {
      await expect(
        run({
          supplied: opening(payload, new Map([["", new Map([["", 1n]])]])),
        }),
      ).rejects.toThrow(/commitment/);
      await expect(
        run({
          supplied: opening(
            payloadFor(kind === "Deposit" ? "Withdrawal" : "Deposit"),
          ),
        }),
      ).rejects.toThrow(/commitment/);
      await expect(
        run({ stateId: { ...id, outputIndex: 3n } }),
      ).rejects.toThrow(/commitment/);
    });
    it("normalizes map wire encoding using the same Plutus commitments", async () => {
      const { aikenSerialisedPlutusDataCborPreservingMapOrder } = await import(
        "@al-ft/midgard-core/plutus-data-cbor"
      );
      const result = await run({
        committedHash: "00".repeat(32),
        supplied: aikenSerialisedPlutusDataCborPreservingMapOrder(
          opening(payload),
        ),
      });
      expect(result.opening).toEqual(
        Data.from(
          opening(payload),
          SDK.FabricatedDepositAuthenticContentOpening,
        ),
      );
    });
  });

it("uses the operator's withdrawal validity separately from body/signature fidelity", async () => {
  const payload = payloadFor("Withdrawal");
  if (!("WithdrawalPayload" in payload)) throw new Error("Wrong fixture kind");
  const info = payload.WithdrawalPayload.event.info;
  expect(
    Effect.runSync(
      SDK.withdrawalContentCommitment({
        ...info,
        validity: "NonExistentWithdrawalUtxo",
      }),
    ),
  ).toBe(Effect.runSync(SDK.withdrawalContentCommitment(info)));
});

it("parses complete opening artifacts and refuses the obsolete datum-only shape", () => {
  for (const parse of [
    parseSubmitFabricatedDepositAuthenticContent,
    parseSubmitFabricatedWithdrawalAuthenticContent,
  ]) {
    expect(parse({ openingCbor: null })).toEqual({ openingCbor: null });
    expect(
      parse({ openingCbor: opening(payloadFor("Deposit")) }).openingCbor,
    ).toBe(opening(payloadFor("Deposit")));
    expect(() => parse({ eventDatumCbor: "d87980" })).toThrow(/exactly/);
  }
});

it("bounds stage transactions before maturity without backdating into the header", () => {
  expect(fabricatedProofValidity(10_000n, 100_000)).toEqual({
    validFrom: 40_000,
    validTo: 220_000,
  });
  expect(fabricatedProofValidity(10_000n, 11_000).validFrom).toBe(11_000);
  expect(() => fabricatedProofValidity(10_000n, 10_000)).toThrow(/window/);
  const deadline = Number(10_000n + SDK.MATURITY_DURATION_MS);
  expect(fabricatedProofValidity(10_000n, deadline - 2000).validTo).toBe(
    deadline,
  );
  expect(() => fabricatedProofValidity(10_000n, deadline - 1000)).toThrow(
    /window/,
  );
});

for (const kind of ["Deposit", "Withdrawal"] as const)
  it.each([false, true])(
    `${kind}: stage-one handoff binds raw source with opaque map: %s`,
    async (rawDatum) => {
      const base = await buildCanonicalBlockFixture({
        transactions: [],
        startTime: 100n,
        endTime: 200n,
      });
      const payload = payloadFor(kind);
      const keyCbor = SDK.committedWithdrawalKeyBytes(id);
      let valueCbor =
        "DepositPayload" in payload
          ? Data.to(payload.DepositPayload.event.info, SDK.DepositInfo)
          : SDK.committedWithdrawalValueBytes(
              payload.WithdrawalPayload.event.info,
            );
      if (rawDatum)
        valueCbor = aikenSerialisedPlutusDataCborPreservingMapOrder(
          replacePlutusConstrFieldCbor(
            valueCbor,
            kind === "Deposit" ? [2] : [0, 4],
            kind === "Deposit"
              ? "d8799fa3020a010b020cff"
              : "d87b9fa3020a010b020cff",
          ),
        );
      const key = Buffer.from(keyCbor, "hex");
      const value = Buffer.from(valueCbor, "hex");
      const root = await buildCountedRoot(
        kind === "Deposit"
          ? SDK.ROOT_DOMAINS.deposits
          : SDK.ROOT_DOMAINS.withdrawals,
        [{ key, value }],
      );
      const proof = await keyValuePhasProof(
        { ...root, root: root.phasRoot },
        key,
        value,
      );
      const header = {
        ...base.header,
        depositsRoot: kind === "Deposit" ? root.root : base.header.depositsRoot,
        depositCount: kind === "Deposit" ? 1n : 0n,
        withdrawalsRoot:
          kind === "Withdrawal" ? root.root : base.header.withdrawalsRoot,
        withdrawalCount: kind === "Withdrawal" ? 1n : 0n,
      };
      const run = (phasRoot: string) =>
        kind === "Deposit"
          ? deriveFabricatedDepositStep01Handoff({
              stateQueuePolicyId: policy,
              header,
              headerHash: "cc".repeat(28),
              inclusion: {
                committedDepositIdCbor: keyCbor,
                committedDepositInfoCbor: valueCbor,
                depositsPhasRoot: phasRoot,
                depositMembershipProof: proof,
                depositMembershipProofCbor: Data.to(proof, SDK.Proof),
              },
            })
          : deriveFabricatedWithdrawalStep01Handoff({
              stateQueuePolicyId: policy,
              header,
              headerHash: "cc".repeat(28),
              inclusion: {
                committedWithdrawalIdCbor: keyCbor,
                committedWithdrawalInfoCbor: valueCbor,
                withdrawalsPhasRoot: phasRoot,
                withdrawalMembershipProof: proof,
                withdrawalMembershipProofCbor: Data.to(proof, SDK.Proof),
              },
            });
      const result = await run(root.phasRoot);
      expect(result.step02State.state_queue_policy).toBe(policy);
      expect(result.step02State.header_end_time).toBe(200n);
      const expectedHash = Effect.runSync(
        kind === "Deposit"
          ? SDK.depositInfoCommitmentCbor(valueCbor)
          : SDK.withdrawalContentCommitmentCbor(valueCbor),
      );
      expect(result.step02State).toMatchObject({
        [kind === "Deposit"
          ? "committed_deposit_info_hash"
          : "committed_withdrawal_content_hash"]: expectedHash,
      });
      await expect(run("ff".repeat(32))).rejects.toThrow(
        "does not open the committed",
      );
    },
  );
