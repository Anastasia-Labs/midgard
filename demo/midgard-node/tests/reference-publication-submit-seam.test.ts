/**
 * Reference publication sends every chained publication tx through the
 * node's one submit seam (I1-fix F7): the seam journals the exact signed
 * bytes as a `reference_publication` intent before the provider sees them,
 * and a journal refusal stops the publication with nothing sent.
 */
import "./helpers/follower-emulator-installed.js";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  IntentJournalRefused,
  type IntentJournalService,
  type RecordOutcome,
} from "../src/services/intent-journal.js";
import { readProviderWalletView } from "../src/services/intent-journal.wallet-view.js";
import { publishReferenceScripts } from "../src/transactions/reference-publication.js";
import { selectNodeWallet } from "../src/transactions/utils.wallet-view.js";
import { TEST_PLAN } from "./helpers/intent-journal.js";

type Event =
  | Readonly<{ kind: "record"; family: string; cbor: string }>
  | Readonly<{ kind: "send"; cbor: string }>;

const scenario = async (refusePublication: boolean) => {
  const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
  const provider = new Emulator([account]);
  const lucid = await Lucid(provider, "Custom");
  selectNodeWallet(lucid, account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, 6)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  const events: Event[] = [];
  const submit = provider.submitTx.bind(provider);
  provider.submitTx = async (cbor) => {
    events.push({ kind: "send", cbor });
    return submit(cbor);
  };
  provider.awaitTx = async () => {
    provider.awaitBlock(1);
    return true;
  };
  const journal: IntentJournalService = {
    openPlan: Effect.succeed(TEST_PLAN),
    record: (intent, signedTxCbor, txHash, _purpose, gate) =>
      intent.kind === "journaled" &&
      intent.family === "reference_publication" &&
      refusePublication
        ? Effect.fail(
            new IntentJournalRefused({
              reason: "intent_input_untracked",
              txHash,
              message: `tx ${txHash} not submitted: refused`,
            }),
          )
        : Effect.sync((): RecordOutcome => {
            events.push({
              kind: "record",
              family: intent.kind === "journaled" ? intent.family : "none",
              cbor: signedTxCbor,
            });
            return { kind: "recorded" };
          }).pipe(
            Effect.zipLeft(
              gate === undefined ? Effect.void : gate(Effect.void),
            ),
          ),
    holds: () => [],
    handOff: () => [],
    adopt: () => undefined,
    refresh: () => Effect.void,
    // No follower here: the view is the provider's UTxOs, read afresh.
    walletView: readProviderWalletView,
  };
  const address = await lucid.wallet().address();
  const run = () =>
    publishReferenceScripts({
      lucid,
      address,
      targets,
      authPolicy,
      reserved: new Set(),
      minAuthPolicyRemainingMs: 1,
      journal,
      options: {
        mode: "serial",
        synchronize: async () => provider.slot,
        wait: async () => {
          provider.awaitBlock(1);
        },
      },
    });
  return { run, events, targets };
};

/** Whether `cbor` carries an authenticated reference-script output. */
const publishes = (cbor: string): boolean => {
  const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
  return Array.from(
    { length: outputs.len() },
    (_, i) => outputs.get(i).script_ref() !== undefined,
  ).some(Boolean);
};

it("journals each publication tx's exact bytes as a reference_publication intent before the provider sees them", async () => {
  const { run, events, targets } = await scenario(false);
  expect(await run()).toHaveLength(targets.length);
  const sends = events.flatMap((event, at) =>
    event.kind === "send" && publishes(event.cbor) ? [{ event, at }] : [],
  );
  expect(sends.length).toBeGreaterThan(0);
  for (const { event, at } of sends)
    expect(
      events
        .slice(0, at)
        .some(
          (prior) =>
            prior.kind === "record" &&
            prior.family === "reference_publication" &&
            prior.cbor === event.cbor,
        ),
    ).toBe(true);
});

it("sends no publication tx the journal refuses, and stops with the refusal", async () => {
  const { run, events } = await scenario(true);
  await expect(run()).rejects.toBeInstanceOf(IntentJournalRefused);
  expect(
    events.some((event) => event.kind === "send" && publishes(event.cbor)),
  ).toBe(false);
});
