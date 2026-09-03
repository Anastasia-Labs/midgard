/**
 * Aiken/TypeScript byte twins for the `fabricated-deposit` family (Goal task
 * `Q39`).
 *
 * Every expectation below is an **absolute** hex constant measured out of the
 * Aiken family modules
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2,3,4}.ak`
 * (via `cbor.serialise` / `utils.serialise_and_hash_32` /
 * `user_events.out_ref_to_nonce` / `transition_trace.commit_counted_root` over
 * that family's own test fixtures). Nothing here compares one TypeScript
 * derivation against another: if either side's encoding moves, the literal stops
 * matching.
 *
 * The family is reached by direct module import rather than through
 * `src/fraud-proof/catalogue.ts`, because the `fabricatedDeposit` catalogue
 * category is not registered yet.
 */
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { OutputReference } from "../src/common.js";
import {
  committedDepositKeyBytes,
  committedDepositValueBytes,
  depositEventDatumCommitment,
  depositEventNonce,
  depositInfoCommitment,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID,
  type FabricatedDepositStep02State,
  FabricatedDepositStep02State as FabricatedDepositStep02StateType,
  type FabricatedDepositStep03State,
  FabricatedDepositStep03State as FabricatedDepositStep03StateType,
  fabricatedDepositStep03State,
  type FabricatedDepositStep04State,
  FabricatedDepositStep04State as FabricatedDepositStep04StateType,
  fabricatedDepositStep04State,
  fabricatedDepositThreadTokenAssetName,
  isFabricatedDepositFault,
} from "../src/fraud-proof/fabricated-deposit-v1.js";
import {
  commitCountedRootProgram,
  ROOT_DOMAINS,
} from "../src/transition-trace.js";
import { DepositDatum } from "../src/user-events/deposit.js";

// ## Fixture twins
//
// `step-01.ak`'s `authentic_deposit_id_v1`, `fabricated_deposit_id_v1`,
// `authentic_deposit_info_v1`, `diverted_deposit_info_v1`, and `step-02.ak`'s
// `authentic_deposit_datum_v1` / `authentic_inclusion_time_v1`.

const AUTHENTIC_DEPOSIT_ID = {
  transactionId:
    "7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a",
  outputIndex: 3n,
};

const FABRICATED_DEPOSIT_ID = {
  transactionId:
    "5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c",
  outputIndex: 0n,
};

const AUTHENTIC_DEPOSIT_INFO = {
  l2_address: {
    paymentCredential: {
      PublicKeyCredential: [
        "1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c",
      ] as [string],
    },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

const DIVERTED_DEPOSIT_INFO = {
  l2_address: {
    paymentCredential: {
      PublicKeyCredential: [
        "2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d",
      ] as [string],
    },
    stakeCredential: null,
  },
  l2_network_id: 0n,
  l2_datum: null,
};

/** `step-02.ak`'s `authentic_inclusion_time_v1`. */
const AUTHENTIC_INCLUSION_TIME = 15n;

const AUTHENTIC_DEPOSIT_EVENT_DATUM = {
  event: { id: AUTHENTIC_DEPOSIT_ID, info: AUTHENTIC_DEPOSIT_INFO },
  inclusion_time: AUTHENTIC_INCLUSION_TIME,
  witness: "57575757575757575757575757575757575757575757575757575757",
};

// ## Measured Aiken constants
//
// The challenged blocks are `step-01.ak`'s `fabricated_identity_block_v1` (FI,
// the nonexistent-identity scenario) and `mismatched_content_block_v1` (MM, the
// content-mismatch scenario). Their header windows are `start_time = 10`,
// `end_time = 20` (inherited from `native_binding_fixture_v1`).

const HEADER_START_TIME = 10n;
const HEADER_END_TIME = 20n;

const KEY_AUTHENTIC_DEPOSIT_ID =
  "d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff";
const KEY_FABRICATED_DEPOSIT_ID =
  "d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ff";

const VALUE_AUTHENTIC_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ff";
const VALUE_DIVERTED_DEPOSIT_INFO =
  "d8799fd8799fd8799f581c2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2dffd87a80ff00d87a80ff";

const HASH_AUTHENTIC_DEPOSIT_INFO =
  "89ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863";
const HASH_DIVERTED_DEPOSIT_INFO =
  "0ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335";

const NONCE_AUTHENTIC_DEPOSIT_ID =
  "db496846395df718772b56f398cc7c7882869ddc0154fd035d63da1c3e95dd06";

const DATUM_AUTHENTIC_DEPOSIT_EVENT =
  "d8799fd8799fd8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd8799fd8799fd8799f581c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1c1cffd87a80ff00d87a80ffff0f581c57575757575757575757575757575757575757575757575757575757ff";
const HASH_AUTHENTIC_DEPOSIT_EVENT_DATUM =
  "2538e7986f6a3468a1dd016318a82d3dd4f60d55f6e688e164dd35564c4a85b4";

const FI_DEPOSITS_PHAS_ROOT =
  "b0374d9482ece991566bebfa200b6577eaeed4a2bcc56e25eb28e8d4f06655b4";
const FI_DEPOSITS_ROOT =
  "60b531d1961d33baf3b6e83da728b0fc1497faf43f78e2cdaf9e03aae9959890";
const FI_HEADER_HASH =
  "3e44a01bc7b6debd95fedbd6851545dc5a31b3eb37db73c30668e119";
const FI_THREAD_TOKEN_ASSET_NAME =
  "0000000b3e44a01bc7b6debd95fedbd6851545dc5a31b3eb37db73c30668e119";

const MM_DEPOSITS_PHAS_ROOT =
  "4b0c3a7234e798d045b06088ab4933c71e22d74781c9457f022987bf8e416c22";
const MM_DEPOSITS_ROOT =
  "880ba7ceb072fce058c5e8f9adbbe9b5bcc3efdcb53ec82039f142f577c47ab4";
const MM_HEADER_HASH =
  "60c9a4c6860d24b6ed3a8f17c4d0718ae0a58cf655bbff24508f7789";
const MM_THREAD_TOKEN_ASSET_NAME =
  "0000000b60c9a4c6860d24b6ed3a8f17c4d0718ae0a58cf655bbff24508f7789";

/** `step_02.State` of the nonexistent-identity scenario. */
const FI_STEP_02_STATE_CBOR =
  "d8799f581c3e44a01bc7b6debd95fedbd6851545dc5a31b3eb37db73c30668e1190a14d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ff582089ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863ff";
/** `step_03.State` of the nonexistent-identity scenario. */
const FI_STEP_03_STATE_CBOR =
  "d8799f581c3e44a01bc7b6debd95fedbd6851545dc5a31b3eb37db73c30668e1190a14d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ff582089ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c6863d87980ff";
/** `step_04.State` of the nonexistent-identity scenario. */
const FI_STEP_04_STATE_CBOR =
  "d8799f581c3e44a01bc7b6debd95fedbd6851545dc5a31b3eb37db73c30668e1190a14d8799f58205c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c5c00ffd87980ff";

/** `step_02.State` of the content-mismatch scenario. */
const MM_STEP_02_STATE_CBOR =
  "d8799f581c60c9a4c6860d24b6ed3a8f17c4d0718ae0a58cf655bbff24508f77890a14d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff58200ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335ff";
/** `step_03.State` of the content-mismatch scenario. */
const MM_STEP_03_STATE_CBOR =
  "d8799f581c60c9a4c6860d24b6ed3a8f17c4d0718ae0a58cf655bbff24508f77890a14d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ff58200ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335d87a9f58202538e7986f6a3468a1dd016318a82d3dd4f60d55f6e688e164dd35564c4a85b40fffff";
/** `step_04.State` of the content-mismatch scenario. */
const MM_STEP_04_STATE_CBOR =
  "d8799f581c60c9a4c6860d24b6ed3a8f17c4d0718ae0a58cf655bbff24508f77890a14d8799f58207a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a03ffd87a9f58200ee4d3827f036188d9d47734f69d3d0db79598a14864eb91595ccbe7f00f8335582089ccb485f7c52cf77b0bdec91ab262a90bc7b519e9b6fae5a2a03529833c68630fffff";

// ## Handoff builders under test

const fiStep02State: FabricatedDepositStep02State = {
  challenged_header_hash: FI_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: FABRICATED_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
};

const mmStep02State: FabricatedDepositStep02State = {
  challenged_header_hash: MM_HEADER_HASH,
  header_start_time: HEADER_START_TIME,
  header_end_time: HEADER_END_TIME,
  committed_deposit_id: AUTHENTIC_DEPOSIT_ID,
  committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
};

const fiStep03State: FabricatedDepositStep03State =
  fabricatedDepositStep03State(fiStep02State, "DepositIdentityAbsent");

const mmStep03State: FabricatedDepositStep03State =
  fabricatedDepositStep03State(mmStep02State, {
    DepositEventObserved: {
      event_datum_hash: HASH_AUTHENTIC_DEPOSIT_EVENT_DATUM,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
    },
  });

const fiStep04State: FabricatedDepositStep04State =
  fabricatedDepositStep04State(fiStep03State, "NonexistentDepositIdentity");

const mmStep04State: FabricatedDepositStep04State =
  fabricatedDepositStep04State(mmStep03State, {
    MismatchedDepositContent: {
      committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
      authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
      event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
    },
  });

describe("fabricated-deposit v1 byte twins", () => {
  it("encodes the committed deposit leaf key exactly as Aiken serialises a DepositId", () => {
    expect(committedDepositKeyBytes(AUTHENTIC_DEPOSIT_ID)).toBe(
      KEY_AUTHENTIC_DEPOSIT_ID,
    );
    expect(committedDepositKeyBytes(FABRICATED_DEPOSIT_ID)).toBe(
      KEY_FABRICATED_DEPOSIT_ID,
    );
    expect(Data.to(AUTHENTIC_DEPOSIT_ID, OutputReference)).toBe(
      KEY_AUTHENTIC_DEPOSIT_ID,
    );
  });

  it("encodes the committed deposit leaf value and its commitment exactly as Aiken does", () => {
    expect(committedDepositValueBytes(AUTHENTIC_DEPOSIT_INFO)).toBe(
      VALUE_AUTHENTIC_DEPOSIT_INFO,
    );
    expect(committedDepositValueBytes(DIVERTED_DEPOSIT_INFO)).toBe(
      VALUE_DIVERTED_DEPOSIT_INFO,
    );
    expect(Effect.runSync(depositInfoCommitment(AUTHENTIC_DEPOSIT_INFO))).toBe(
      HASH_AUTHENTIC_DEPOSIT_INFO,
    );
    expect(Effect.runSync(depositInfoCommitment(DIVERTED_DEPOSIT_INFO))).toBe(
      HASH_DIVERTED_DEPOSIT_INFO,
    );
    expect(HASH_AUTHENTIC_DEPOSIT_INFO).not.toBe(HASH_DIVERTED_DEPOSIT_INFO);
  });

  it("derives the deposit event NFT nonce exactly as Aiken's out_ref_to_nonce does", () => {
    expect(Effect.runSync(depositEventNonce(AUTHENTIC_DEPOSIT_ID))).toBe(
      NONCE_AUTHENTIC_DEPOSIT_ID,
    );
  });

  it("encodes the authentic deposit event datum and step-02's retained commitment exactly as Aiken does", () => {
    expect(Data.to(AUTHENTIC_DEPOSIT_EVENT_DATUM, DepositDatum)).toBe(
      DATUM_AUTHENTIC_DEPOSIT_EVENT,
    );
    expect(
      Effect.runSync(
        depositEventDatumCommitment(AUTHENTIC_DEPOSIT_EVENT_DATUM),
      ),
    ).toBe(HASH_AUTHENTIC_DEPOSIT_EVENT_DATUM);
  });

  it("commits the challenged headers' counted deposits_root and thread token asset names exactly as Aiken does", () => {
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.deposits,
          phasRoot: FI_DEPOSITS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(FI_DEPOSITS_ROOT);
    expect(
      Effect.runSync(
        commitCountedRootProgram({
          domain: ROOT_DOMAINS.deposits,
          phasRoot: MM_DEPOSITS_PHAS_ROOT,
          count: 1n,
        }),
      ),
    ).toBe(MM_DEPOSITS_ROOT);
    expect(FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID).toBe("0000000b");
    expect(fabricatedDepositThreadTokenAssetName(FI_HEADER_HASH)).toBe(
      FI_THREAD_TOKEN_ASSET_NAME,
    );
    expect(fabricatedDepositThreadTokenAssetName(MM_HEADER_HASH)).toBe(
      MM_THREAD_TOKEN_ASSET_NAME,
    );
  });

  it("encodes the step-01 to step-02 handoff exactly as the Aiken step-01 validator produces it", () => {
    expect(Data.to(fiStep02State, FabricatedDepositStep02StateType)).toBe(
      FI_STEP_02_STATE_CBOR,
    );
    expect(Data.to(mmStep02State, FabricatedDepositStep02StateType)).toBe(
      MM_STEP_02_STATE_CBOR,
    );
  });

  it("encodes the step-02 to step-03 handoff exactly as the Aiken step-02 validator produces it", () => {
    expect(Data.to(fiStep03State, FabricatedDepositStep03StateType)).toBe(
      FI_STEP_03_STATE_CBOR,
    );
    expect(Data.to(mmStep03State, FabricatedDepositStep03StateType)).toBe(
      MM_STEP_03_STATE_CBOR,
    );
  });

  it("encodes the step-03 to step-04 handoff and settles the fault rule exactly as the Aiken step-04 validator does", () => {
    expect(Data.to(fiStep04State, FabricatedDepositStep04StateType)).toBe(
      FI_STEP_04_STATE_CBOR,
    );
    expect(Data.to(mmStep04State, FabricatedDepositStep04StateType)).toBe(
      MM_STEP_04_STATE_CBOR,
    );
    // The rule twin of `fabricated_deposit_fault_is_established_v1`.
    expect(isFabricatedDepositFault(fiStep04State)).toBe(true);
    expect(isFabricatedDepositFault(mmStep04State)).toBe(true);
    // A header committing exactly the authentic content is not a fault, and an
    // authentic event outside the challenged block's window is not this block's
    // fault, whichever side of the window it falls on.
    expect(
      isFabricatedDepositFault({
        ...mmStep04State,
        fault: {
          MismatchedDepositContent: {
            committed_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
            authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
            event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedDepositFault({
        ...mmStep04State,
        fault: {
          MismatchedDepositContent: {
            committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
            authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
            event_inclusion_time: HEADER_END_TIME + 1n,
          },
        },
      }),
    ).toBe(false);
    expect(
      isFabricatedDepositFault({
        ...mmStep04State,
        fault: {
          MismatchedDepositContent: {
            committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
            authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
            event_inclusion_time: HEADER_START_TIME,
          },
        },
      }),
    ).toBe(false);
  });
});
