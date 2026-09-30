import "./fabricated-withdrawal.q40-fabricated-withdrawal-l1-witness-authentication.js";

import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { authenticateFabricatedHistoryWitness } from "../src/fabricated-history-witness.js";
import { prepareFabricatedWithdrawalFromCommittedLeaves } from "../src/prepare-fabricated-withdrawal.js";
import {
  deriveFabricatedWithdrawalStep01Handoff,
  parseSubmitFabricatedWithdrawalInclusion,
} from "../src/submit-fabricated-withdrawal-step-01.js";
import { deriveFabricatedWithdrawalStep03Handoff } from "../src/submit-fabricated-withdrawal-step-03.js";
import { assertFabricatedWithdrawalStep04Finalizable } from "../src/submit-fabricated-withdrawal-step-04.js";
import {
  AUTHENTIC_INCLUSION_TIME,
  AUTHENTIC_WITHDRAWAL_ID,
  buildWithdrawalsBlockFixture,
  DATUM_AUTHENTIC_WITHDRAWAL_EVENT,
  DEPOSIT_POLICY_ID,
  FABRICATED_WITHDRAWAL_ID,
  FI_HEADER_HASH,
  FI_STEP_04_STATE_CBOR,
  FI_WITHDRAWALS_PHAS_ROOT,
  HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  HASH_DIVERTED_WITHDRAWAL_CONTENT,
  HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
  HEADER_END_TIME,
  HEADER_START_TIME,
  historyWitness,
  hubOracleUtxoFixture,
  MM_HEADER_HASH,
  MM_LEAF,
  MM_STEP_04_STATE_CBOR,
  MM_WITHDRAWALS_PHAS_ROOT,
  MM_WITHDRAWALS_ROOT,
  NONCE_AUTHENTIC_WITHDRAWAL_ID,
  WITHDRAWAL_POLICY_ID,
} from "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import {
  authenticWithdrawalInfo,
  fiStep03State,
  historyAssets,
  historyCommitment,
  historyOpening,
  l1AddressOf,
  mmStep03State,
  presentEventWitness,
  withdrawalEventDatum,
  withdrawalEventUtxoFixture,
} from "./fabricated-withdrawal.q40-fabricated-withdrawal-evidence-admission.js";
import { h28 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q40 fabricated-withdrawal submit-side re-derivation", () => {
  it("re-derives the step-01 handoff from the on-chain header and refuses a PHAS root or leaf encoding that does not open it", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    const inclusion = parseSubmitFabricatedWithdrawalInclusion(
      plan.withdrawalInclusion,
    );
    const handoff = await deriveFabricatedWithdrawalStep01Handoff({
      stateQueuePolicyId: h28(0x15),
      header: fixture.header,
      headerHash: fixture.headerHash,
      inclusion,
    });
    expect(handoff.committedWithdrawal.domain).toBe(
      SDK.ROOT_DOMAINS.withdrawals,
    );
    expect(handoff.committedWithdrawal.root).toBe(MM_WITHDRAWALS_ROOT);
    expect(handoff.committedWithdrawal.phas_root).toBe(
      MM_WITHDRAWALS_PHAS_ROOT,
    );
    expect(handoff.committedWithdrawal.count).toBe(1n);
    expect(handoff.step02State).toEqual({
      state_queue_policy: h28(0x15),
      challenged_header_hash: fixture.headerHash,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_withdrawal_id: AUTHENTIC_WITHDRAWAL_ID,
      committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
    });

    await expect(
      deriveFabricatedWithdrawalStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: {
          ...inclusion,
          withdrawalsPhasRoot: FI_WITHDRAWALS_PHAS_ROOT,
        },
      }),
    ).rejects.toThrow(/does not open the committed withdrawals_root/u);

    // The submit side refuses leaf bytes in Lucid's indefinite-map form too: the
    // membership check on chain hashes the `serialise_data` bytes.
    await expect(
      deriveFabricatedWithdrawalStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: {
          ...inclusion,
          committedWithdrawalInfoCbor: Data.to(
            {
              ...authenticWithdrawalInfo(),
              body: {
                ...authenticWithdrawalInfo().body,
                l1_address: l1AddressOf(0x5d),
              },
            },
            SDK.WithdrawalInfo,
          ),
        },
      }),
    ).rejects.toThrow(/is not in serialiseData form/u);
  });

  it("authenticates the withdrawal event UTxO through the hub oracle's withdrawal policy, not its deposit policy", async () => {
    const authenticate = (anchor: UTxO) =>
      authenticateFabricatedHistoryWitness(
        historyWitness(anchor),
        "Withdrawal",
        AUTHENTIC_WITHDRAWAL_ID,
      );
    const authenticated = await authenticate(withdrawalEventUtxoFixture());
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...historyWitness(withdrawalEventUtxoFixture()),
          hubOracleUtxo: {
            ...hubOracleUtxoFixture(),
            assets: { lovelace: 5_000_000n },
          },
        },
        "Withdrawal",
        AUTHENTIC_WITHDRAWAL_ID,
      ),
    ).rejects.toThrow("authentic inline hub oracle");
    await expect(
      authenticate({
        ...withdrawalEventUtxoFixture(),
        address: credentialToAddress("Preview", {
          type: "Key",
          hash: h28(0x44),
        }),
      }),
    ).rejects.toThrow("Invalid authenticated history output shape");

    expect(authenticated.deployment.policyId).toBe(WITHDRAWAL_POLICY_ID);
    expect(authenticated.witness.anchor.key).toBe(
      NONCE_AUTHENTIC_WITHDRAWAL_ID,
    );
    expect(authenticated.captured?.commitment).toEqual({
      ...historyCommitment,
      policy: WITHDRAWAL_POLICY_ID,
    });
    expect(authenticated.captured?.originalAssets).toEqual(historyAssets);

    // The hub oracle's *deposit* policy is a different event family's policy, so
    // an event NFT minted under it is not an authentic withdrawal event even
    // though the asset name is the authentic nonce.
    await expect(
      authenticate(withdrawalEventUtxoFixture({ policyId: DEPOSIT_POLICY_ID })),
    ).rejects.toThrow(/no unique authenticated witness/u);
    // The authentic policy and nonce, but a datum for another identity.
    await expect(
      authenticate(
        withdrawalEventUtxoFixture({
          datum: withdrawalEventDatum({ id: FABRICATED_WITHDRAWAL_ID }),
        }),
      ),
    ).rejects.toThrow(/identity differs/u);
  });

  it("opens step-02's retained commitment into the Aiken scenarios' exact step-04 handoffs, for both fidelity fabrications", async () => {
    const absent = await deriveFabricatedWithdrawalStep03Handoff({
      state: fiStep03State,
    });
    expect(absent.opening).toBe("NoAuthenticContent");
    expect(absent.fault).toBe("NonexistentWithdrawalIdentity");
    expect(
      Data.to(absent.step04State, SDK.FabricatedWithdrawalStep04State),
    ).toBe(FI_STEP_04_STATE_CBOR);

    const present = await deriveFabricatedWithdrawalStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(),
    });
    expect(present.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(
      Data.to(present.step04State, SDK.FabricatedWithdrawalStep04State),
    ).toBe(MM_STEP_04_STATE_CBOR);

    // One 32-byte inequality settles body and signature: a forged signature
    // convicts on the same rule as the diverted payout body.
    const forged = await deriveFabricatedWithdrawalStep03Handoff({
      state: {
        ...mmStep03State,
        committed_withdrawal_content_hash:
          HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
      },
      openingCbor: historyOpening(),
    });
    expect(forged.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash:
          HASH_FORGED_SIGNATURE_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });

    // Lucid's indefinite-map wire form of the same event datum is accepted,
    // because the on-chain step re-serialises the redeemer before hashing it —
    // the one place this family must *not* demand byte-identical CBOR.
    const rawLucidDatum = Data.to(
      withdrawalEventDatum(),
      SDK.WithdrawalOrderDatum,
    );
    expect(rawLucidDatum).not.toBe(DATUM_AUTHENTIC_WITHDRAWAL_EVENT);
    const normalized = await deriveFabricatedWithdrawalStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(
        Data.from(rawLucidDatum, SDK.WithdrawalOrderDatum),
      ),
    });
    expect(normalized.fault).toEqual(present.fault);
  });

  it("refuses step-03 openings that do not pair with the verdict or are not the authenticated bytes, and refuses to finalize a misfiled or unestablished conviction", async () => {
    // A present-event verdict opened as an absence would convert a content
    // dispute into the strictly stronger non-existence conviction.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({ state: mmStep03State }),
    ).rejects.toThrow(/requires its retained payload and original Value/u);
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: fiStep03State,
        openingCbor: historyOpening(),
      }),
    ).rejects.toThrow(/absence admits no retained event opening/u);
    // Only the hash equality makes supplied bytes authentic.
    await expect(
      deriveFabricatedWithdrawalStep03Handoff({
        state: mmStep03State,
        openingCbor: historyOpening(
          withdrawalEventDatum(),
          new Map([["", new Map([["", 3_000_001n]])]]),
        ),
      }),
    ).rejects.toThrow(/does not match the authenticated history commitment/u);

    const established = SDK.fabricatedWithdrawalStep04State(mmStep03State, {
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: established,
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).not.toThrow();
    // Filed against a header the thread token does not name.
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: established,
        fraudulentHeaderHash: FI_HEADER_HASH,
      }),
    ).toThrow(/thread state names challenged header/u);
    // An authentic event outside the challenged block's window is not this
    // block's fault, so it can never become a permanent conviction.
    expect(() =>
      assertFabricatedWithdrawalStep04Finalizable({
        state: {
          ...established,
          fault: {
            MismatchedWithdrawalContent: {
              committed_withdrawal_content_hash:
                HASH_DIVERTED_WITHDRAWAL_CONTENT,
              authentic_withdrawal_content_hash:
                HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
              event_inclusion_time: HEADER_END_TIME + 1n,
            },
          },
        },
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).toThrow(/not an established fabricated-withdrawal fault/u);
  });
});
