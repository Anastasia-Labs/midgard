import { generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { publishWorkflowDeployment } from "midgard-node/tests/helpers/published-workflow-deployment";
import { expect, it } from "vitest";

import { createPublishedWatcherBlockActor } from "./support/published-block-actor.js";

// On an attached stack the drill's funding wallet pays for the journey, while
// the stack published its reference scripts from its own reference wallet,
// the address the deployment manifest records. Onboarding must find the
// deployment's reference scripts there, not at whichever wallet funds it.
it("onboards an operator against the deployment's reference scripts when another wallet funds the journey", async () => {
  const deployment = await publishWorkflowDeployment();
  const funding = await Lucid(
    deployment.emulator,
    deployment.operatorLucid.config().network,
  );
  funding.selectWallet.fromSeed(generateSeedPhrase());
  const fundingAddress = await funding.wallet().address();
  expect(fundingAddress).not.toBe(
    deployment.manifest.referenceScriptDeployAddress,
  );

  const actor = await createPublishedWatcherBlockActor({
    deployment: { ...deployment, publisherLucid: funding },
    lucid: deployment.operatorLucid,
  });
  await actor.onboardOperator();

  expect(await actor.operatorActive()).toBe(true);
}, 900_000);
