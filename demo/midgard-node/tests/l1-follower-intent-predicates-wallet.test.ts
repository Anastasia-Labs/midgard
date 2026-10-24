/**
 * The node's §8.4 predicates of the reference-script and stake-registration
 * families over the follower's facts (I1, E1/E2), each in both polarities,
 * on the node database:
 *
 * - stake registration (`script_reward_registration`, `phas_membership`):
 *   wanted while no canonical landed transaction's certificate shows the
 *   credential its own certificate registers as registered (the latest
 *   certificate decides); a dropped registration is resent, a landed one is
 *   not, and one the ledger refuses holds its family until its input is
 *   spent;
 * - reference funding: a working-capital top-up is wanted while the plain
 *   balance at the address its key names is below its target; a
 *   publication's funding step while a script its content reference names
 *   is not yet published;
 * - reference publication: wanted while a script it publishes is not live
 *   at another output; reference sweep: wanted while it spends a live
 *   reference-script output and every input is live.
 */
import {
  blake2b224,
  type FactStore,
  liveUtxosIn,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  cbor as c,
  encodeSimTx,
  type SimTx,
} from "@al-ft/midgard-l1-follower/testing";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { nodeFamilyPredicate } from "../src/services/l1-follower.intent-predicates.js";
import {
  createNodeIntentStage,
  INTENT_RESUBMIT_REJECTED,
} from "../src/services/l1-follower.intents.js";
import {
  openPredicateScenario,
  type PredicateScenario,
} from "./helpers/intent-predicates-scenario.js";
import {
  loadOperatorSetChainFixture,
  type OperatorSetChainFixture,
} from "./helpers/operator-set-chain.js";
import { QUEUE_ADDRESS } from "./helpers/state-queue-sim.fixtures.js";

const opened: FactStore[] = [];
let fixture: OperatorSetChainFixture;
beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

const open = () => openPredicateScenario(fixture, opened);

/** A script credential `[1, hash]`. */
const scriptCredential = (byte: number) =>
  c.array(c.uint(1), c.bytes(Buffer.alloc(28, byte)));

/** `reg_cert` (tag 7), `stake_registration` (0), `unreg_cert` (8). */
const regCert = (byte: number) =>
  c.array(c.uint(7), scriptCredential(byte), c.uint(2_000_000));
const legacyRegCert = (byte: number) =>
  c.array(c.uint(0), scriptCredential(byte));
const unregCert = (byte: number) =>
  c.array(c.uint(8), scriptCredential(byte), c.uint(2_000_000));
/** A delegation (tag 2): no registration change. */
const delegationCert = (byte: number) =>
  c.array(c.uint(2), scriptCredential(byte), c.bytes(Buffer.alloc(28, 0x99)));

const FAMILIES = ["script_reward_registration", "phas_membership"] as const;

/** A tracked transaction spending a fresh spare, with `extra`. */
const other = (s: PredicateScenario, spare: OutRef, extra: Partial<SimTx>) =>
  s.spend([spare], { nonce: s.nonce(), ...extra });

const PLUTUS = Buffer.from("4e4d01000033222220051200120011", "hex");
const plutusHash = (body: Buffer): Buffer =>
  blake2b224(Buffer.concat([Buffer.of(3), body]));

describe("the node's reference-script and stake-registration predicates over the follower's facts", () => {
  it.each(FAMILIES)(
    "%s: wanted while no landed certificate shows its credential registered; the latest certificate decides",
    async (family) => {
      const s = await open();
      const intent = await s.record(
        family,
        `${family}:test`,
        s.spend([s.spare(0)], {
          certificates: [delegationCert(0x51), regCert(0x51)],
        }),
        null,
      );
      // The credential's state predates the facts: wanted (resent).
      expect(await s.verdict(intent)).toBe(true);
      // A delegation of the credential changes nothing.
      await s.driver.forward([
        other(s, s.spare(1), { certificates: [delegationCert(0x51)] }),
      ]);
      expect(await s.verdict(intent)).toBe(true);
      // Another landed transaction registers it (the pre-Conway tag): not wanted.
      await s.driver.forward([
        other(s, s.spare(2), { certificates: [legacyRegCert(0x51)] }),
      ]);
      expect(await s.verdict(intent)).toBe(false);
      // A later deregistration: wanted again.
      await s.driver.forward([
        other(s, s.spare(3), { certificates: [unregCert(0x51)] }),
      ]);
      expect(await s.verdict(intent)).toBe(true);
      // A registration of another credential leaves it wanted.
      await s.driver.forward([
        other(s, s.spare(4), { certificates: [regCert(0x52)] }),
      ]);
      expect(await s.verdict(intent)).toBe(true);
      await s.driver.forward([
        other(s, s.spare(5), { certificates: [regCert(0x51)] }),
      ]);
      expect(await s.verdict(intent)).toBe(false);
    },
  );

  it("a registration intent whose body registers no credential throws (held)", async () => {
    const s = await open();
    const intent = await s.record(
      "script_reward_registration",
      "script_reward_registration:test",
      s.spend([s.spare(0)]),
      null,
    );
    await expect(s.verdict(intent)).rejects.toThrow(
      "registers no stake credential",
    );
  });

  it("a dropped registration is resent, a landed one is not, and one the ledger refuses holds its family until its input is spent", async () => {
    const s = await open();
    const sent: Buffer[] = [];
    const refuse = new Set<string>();
    const stage = createNodeIntentStage({
      store: s.store,
      transport: {
        hasTx: () => Promise.resolve(false),
        submit: (bytes) => {
          sent.push(Buffer.from(bytes));
          return Promise.resolve(
            refuse.has(Buffer.from(bytes).toString("hex"))
              ? { accepted: false, rejection: Buffer.from("refused") }
              : { accepted: true },
          );
        },
        withLedgerState: () => Promise.reject(new Error("no ledger state")),
      },
      securityParameter: 6,
      seededAddresses: [],
      wanted: nodeFamilyPredicate(s.deps()),
      log: () => {},
    });
    const landing = s.spend([s.spare(0)], { certificates: [regCert(0x61)] });
    const refused = s.spend([s.spare(1)], { certificates: [regCert(0x62)] });
    await s.record("phas_membership", "phas_membership:a", landing, null);
    await s.record("phas_membership", "phas_membership:b", refused, null);
    refuse.add(encodeSimTx(refused).toString("hex"));

    expect(await stage.run()).toEqual([]);
    expect(sent.map((bytes) => bytes.toString("hex")).sort()).toEqual(
      [encodeSimTx(landing), encodeSimTx(refused)]
        .map((bytes) => bytes.toString("hex"))
        .sort(),
    );
    // The first lands; the refused one is refused at a second tip.
    await s.driver.forward([landing]);
    sent.length = 0;
    const held = await stage.run();
    expect(sent).toEqual([encodeSimTx(refused)]);
    expect(held.map((hold) => hold.reason)).toEqual([INTENT_RESUBMIT_REJECTED]);
    // Another landed transaction spends the refused intent's input: it is
    // conflicted, never sent again, and the hold clears.
    await s.driver.forward([other(s, s.spare(1), {})]);
    sent.length = 0;
    expect(await stage.run()).toEqual([]);
    expect(sent).toEqual([]);
    stage.close();
  });

  it("working-capital funding: wanted while the plain balance at the named address is below the target", async () => {
    const s = await open();
    const live = await s.store.transaction("read", (tx) =>
      liveUtxosIn(tx, s.store.dialect, {
        by: "address",
        address: QUEUE_ADDRESS,
      }),
    );
    if (live.kind !== "ok") throw new Error(live.kind);
    const plain = live.utxos
      .filter(
        (utxo) =>
          utxo.output.assets.size === 0 &&
          utxo.output.datum === null &&
          utxo.output.scriptRef === null,
      )
      .reduce((sum, utxo) => sum + utxo.output.lovelace, 0n);
    const target = plain + 5_000_000n;
    const funding = await s.record(
      "reference_funding",
      `reference_funding:node-runtime:${QUEUE_ADDRESS.toString("hex")}:${target.toString()}`,
      s.spend([s.spare(0)]),
      null,
    );
    expect(await s.verdict(funding)).toBe(true);
    // A payment that is not plain (it carries a datum) does not count.
    await s.driver.forward([
      {
        inputs: [s.driver.chain.outsideInput()],
        outputs: [
          {
            address: QUEUE_ADDRESS,
            lovelace: 9_000_000n,
            datum: Buffer.from("00", "hex"),
          },
        ],
        nonce: s.nonce(),
      },
    ]);
    expect(await s.verdict(funding)).toBe(true);
    // Another transaction tops the address up past the target.
    await s.driver.forward([
      {
        inputs: [s.driver.chain.outsideInput()],
        outputs: [{ address: QUEUE_ADDRESS, lovelace: 5_000_000n }],
        nonce: s.nonce(),
      },
    ]);
    expect(await s.verdict(funding)).toBe(false);
    const malformed = await s.record(
      "reference_funding",
      "reference_funding:node-runtime",
      s.spend([s.spare(1)]),
      null,
    );
    await expect(s.verdict(malformed)).rejects.toThrow(
      "names no address and target",
    );
  });

  it("publication funding: wanted while a script it funds is not yet published", async () => {
    const s = await open();
    const other = Buffer.from("4e4d01000033222220051200120012", "hex");
    const funding = await s.record(
      "reference_funding",
      "reference_publication:split",
      s.spend([s.spare(0)]),
      Buffer.concat([plutusHash(PLUTUS), plutusHash(other)]),
    );
    expect(await s.verdict(funding)).toBe(true);
    await s.driver.forward([
      {
        inputs: [s.driver.chain.outsideInput()],
        outputs: [
          { address: QUEUE_ADDRESS, lovelace: 9_000_000n, scriptRef: PLUTUS },
        ],
        nonce: s.nonce(),
      },
    ]);
    // One of the two is published: still wanted.
    expect(await s.verdict(funding)).toBe(true);
    await s.driver.forward([
      {
        inputs: [s.driver.chain.outsideInput()],
        outputs: [
          { address: QUEUE_ADDRESS, lovelace: 9_000_000n, scriptRef: other },
        ],
        nonce: s.nonce(),
      },
    ]);
    expect(await s.verdict(funding)).toBe(false);
  });

  it("reference publication: wanted while a script it publishes is live at no other output", async () => {
    const s = await open();
    const publication = await s.record(
      "reference_publication",
      "reference_publication:test",
      s.spend([s.spare(0)], {
        outputs: [
          { address: QUEUE_ADDRESS, lovelace: 9_000_000n, scriptRef: PLUTUS },
        ],
      }),
      null,
    );
    expect(await s.verdict(publication)).toBe(true);
    // The same script is already live elsewhere: not wanted.
    await s.driver.forward([
      other(s, s.spare(1), {
        outputs: [
          { address: QUEUE_ADDRESS, lovelace: 9_000_000n, scriptRef: PLUTUS },
        ],
      }),
    ]);
    expect(await s.verdict(publication)).toBe(false);
  });

  it("reference sweep: wanted while it spends a live reference-script output; one whose inputs carry no script is not", async () => {
    const s = await open();
    const [published] = await s.driver.forward([
      other(s, s.spare(0), {
        outputs: [
          { address: QUEUE_ADDRESS, lovelace: 9_000_000n, scriptRef: PLUTUS },
        ],
      }),
    ]);
    const sweep = await s.record(
      "reference_sweep",
      "reference_sweep:test",
      s.spend([{ txHash: published!, index: 0 }, s.spare(1)]),
      null,
    );
    expect(await s.verdict(sweep)).toBe(true);
    const plainSweep = await s.record(
      "reference_sweep",
      "reference_sweep:plain",
      s.spend([s.spare(2)]),
      null,
    );
    expect(await s.verdict(plainSweep)).toBe(false);
  });
});
