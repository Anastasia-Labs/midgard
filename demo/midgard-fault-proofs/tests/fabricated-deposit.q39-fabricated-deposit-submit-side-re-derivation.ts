import "./fabricated-deposit.q39-fabricated-deposit-production-evidence-authority.js";

import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { authenticateFabricatedHistoryWitness } from "../src/fabricated-history-witness.js";
import { prepareFabricatedDepositFromCommittedLeaves } from "../src/prepare-fabricated-deposit.js";
import {
  deriveFabricatedDepositStep01Handoff,
  parseSubmitFabricatedDepositInclusion,
} from "../src/submit-fabricated-deposit-step-01.js";
import { deriveFabricatedDepositStep03Handoff } from "../src/submit-fabricated-deposit-step-03.js";
import { assertFabricatedDepositStep04Finalizable } from "../src/submit-fabricated-deposit-step-04.js";
import {
  AUTHENTIC_DEPOSIT_ID,
  AUTHENTIC_INCLUSION_TIME,
  buildDepositsBlockFixture,
  DEPOSIT_POLICY_ID,
  FABRICATED_DEPOSIT_ID,
  FI_DEPOSITS_PHAS_ROOT,
  FI_HEADER_HASH,
  FI_STEP_04_STATE_CBOR,
  HASH_AUTHENTIC_DEPOSIT_INFO,
  HASH_DIVERTED_DEPOSIT_INFO,
  HEADER_END_TIME,
  HEADER_START_TIME,
  historyWitness,
  hubOracleUtxoFixture,
  MM_DEPOSITS_PHAS_ROOT,
  MM_DEPOSITS_ROOT,
  MM_HEADER_HASH,
  MM_LEAF,
  MM_STEP_04_STATE_CBOR,
  NONCE_AUTHENTIC_DEPOSIT_ID,
} from "./fabricated-deposit.build-deposits-block-fixture.js";
import {
  depositEventDatum,
  depositEventUtxoFixture,
  fiStep03State,
  historyAssets,
  historyCommitment,
  historyOpening,
  mmStep03State,
  presentEventWitness,
} from "./fabricated-deposit.q39-fabricated-deposit-evidence-admission.js";
import { h28 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q39 fabricated-deposit submit-side re-derivation", () => {
  it("re-derives the step-01 handoff from the on-chain header and refuses a PHAS root that does not open it", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    const inclusion = parseSubmitFabricatedDepositInclusion(
      plan.depositInclusion,
    );
    const handoff = await deriveFabricatedDepositStep01Handoff({
      stateQueuePolicyId: h28(0x15),
      header: fixture.header,
      headerHash: fixture.headerHash,
      inclusion,
    });
    expect(handoff.committedDeposit.domain).toBe(SDK.ROOT_DOMAINS.deposits);
    expect(handoff.committedDeposit.root).toBe(MM_DEPOSITS_ROOT);
    expect(handoff.committedDeposit.phas_root).toBe(MM_DEPOSITS_PHAS_ROOT);
    expect(handoff.committedDeposit.count).toBe(1n);
    expect(handoff.step02State).toEqual({
      state_queue_policy: h28(0x15),
      challenged_header_hash: fixture.headerHash,
      header_start_time: HEADER_START_TIME,
      header_end_time: HEADER_END_TIME,
      committed_deposit_id: AUTHENTIC_DEPOSIT_ID,
      committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
    });

    await expect(
      deriveFabricatedDepositStep01Handoff({
        stateQueuePolicyId: h28(0x15),
        header: fixture.header,
        headerHash: fixture.headerHash,
        inclusion: { ...inclusion, depositsPhasRoot: FI_DEPOSITS_PHAS_ROOT },
      }),
    ).rejects.toThrow(/does not open the committed deposits_root/u);
  });

  it("authenticates the deposit event UTxO through the hub oracle's deposit policy", async () => {
    const authenticate = (anchor: UTxO) =>
      authenticateFabricatedHistoryWitness(
        historyWitness(anchor),
        "Deposit",
        AUTHENTIC_DEPOSIT_ID,
      );
    const authenticated = await authenticate(depositEventUtxoFixture());
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...historyWitness(depositEventUtxoFixture()),
          hubOracleUtxo: {
            ...hubOracleUtxoFixture(),
            assets: { lovelace: 5_000_000n },
          },
        },
        "Deposit",
        AUTHENTIC_DEPOSIT_ID,
      ),
    ).rejects.toThrow("authentic inline hub oracle");
    await expect(
      authenticate({
        ...depositEventUtxoFixture(),
        address: credentialToAddress("Preview", {
          type: "Key",
          hash: h28(0x44),
        }),
      }),
    ).rejects.toThrow("Invalid authenticated history output shape");

    expect(authenticated.deployment.policyId).toBe(DEPOSIT_POLICY_ID);
    expect(authenticated.witness.anchor.key).toBe(NONCE_AUTHENTIC_DEPOSIT_ID);
    expect(authenticated.captured?.commitment).toEqual({
      ...historyCommitment,
      policy: DEPOSIT_POLICY_ID,
    });
    expect(authenticated.captured?.originalAssets).toEqual(historyAssets);

    // A foreign policy is refused even though the asset name is the authentic
    // nonce: the policy comes from the hub oracle, not from the prover.
    await expect(
      authenticate(depositEventUtxoFixture({ policyId: h28(0x99) })),
    ).rejects.toThrow(/no unique authenticated witness/u);
    // The authentic policy and nonce, but a datum for another identity.
    await expect(
      authenticate(
        depositEventUtxoFixture({
          datum: depositEventDatum({ id: FABRICATED_DEPOSIT_ID }),
        }),
      ),
    ).rejects.toThrow(/identity differs/u);
  });

  it("opens step-02's retained commitment into the Aiken scenarios' exact step-04 handoffs", async () => {
    const absent = await deriveFabricatedDepositStep03Handoff({
      state: fiStep03State,
    });
    expect(absent.opening).toBe("NoAuthenticContent");
    expect(absent.fault).toBe("NonexistentDepositIdentity");
    expect(Data.to(absent.step04State, SDK.FabricatedDepositStep04State)).toBe(
      FI_STEP_04_STATE_CBOR,
    );

    const present = await deriveFabricatedDepositStep03Handoff({
      state: mmStep03State,
      openingCbor: historyOpening(),
    });
    expect(present.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(Data.to(present.step04State, SDK.FabricatedDepositStep04State)).toBe(
      MM_STEP_04_STATE_CBOR,
    );
  });

  it("refuses step-03 openings that do not pair with the verdict or are not the authenticated bytes", async () => {
    // A present-event verdict opened as an absence would convert a content
    // dispute into the strictly stronger non-existence conviction.
    await expect(
      deriveFabricatedDepositStep03Handoff({ state: mmStep03State }),
    ).rejects.toThrow(/requires its retained payload and original Value/u);
    await expect(
      deriveFabricatedDepositStep03Handoff({
        state: fiStep03State,
        openingCbor: historyOpening(),
      }),
    ).rejects.toThrow(/absence admits no retained event opening/u);
    // Only the hash equality makes supplied bytes authentic.
    await expect(
      deriveFabricatedDepositStep03Handoff({
        state: mmStep03State,
        openingCbor: historyOpening(
          depositEventDatum({ paymentKeyByte: 0x3e }),
        ),
      }),
    ).rejects.toThrow(/does not match the authenticated history commitment/u);
  });

  it("refuses to finalize a misfiled conviction or an unestablished fault", async () => {
    const established: SDK.FabricatedDepositStep04State =
      SDK.fabricatedDepositStep04State(mmStep03State, {
        MismatchedDepositContent: {
          committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
          authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
          event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
        },
      });
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: established,
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).not.toThrow();
    // Filed against a header the thread token does not name.
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: established,
        fraudulentHeaderHash: FI_HEADER_HASH,
      }),
    ).toThrow(/thread state names challenged header/u);
    // An authentic event outside the challenged block's window is not this
    // block's fault, so it can never become a permanent conviction.
    expect(() =>
      assertFabricatedDepositStep04Finalizable({
        state: {
          ...established,
          fault: {
            MismatchedDepositContent: {
              committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
              authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
              event_inclusion_time: HEADER_END_TIME + 1n,
            },
          },
        },
        fraudulentHeaderHash: MM_HEADER_HASH,
      }),
    ).toThrow(/not an established fabricated-deposit fault/u);
  });
});
