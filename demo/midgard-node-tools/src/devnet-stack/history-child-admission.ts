import { codeStamp, runtimeDistTargets } from "./dist-freshness.js";
import { retryHistoryAdmission } from "./history-admission-retry.js";
import type { HistoryChildRole } from "./history-child-evidence.js";
import { HistoryConfigurationRefusal } from "./history-configuration-refusal.js";
import { readHistoryChildContext as readContext } from "./history-role-context.js";
import { makeHistorySignedAdmission } from "./history-signed-admission.js";
import type { Layout, RunEnv } from "./layout.js";
import { readRunEnv } from "./layout.js";

/** Admission precedes every role constructor; inherited declarations are not proof. */
export const admitHistoryChild = async (
  role: HistoryChildRole,
  layout: Layout,
  run: RunEnv,
  signal: AbortSignal,
  context = readContext(role, run),
) => {
  const currentScope = () => {
    const actualRun = readRunEnv(layout);
    if (
      actualRun.runId !== run.runId ||
      actualRun.networkMagic !== run.networkMagic ||
      actualRun.portOffset !== run.portOffset
    )
      throw new HistoryConfigurationRefusal(
        "history child public run binding changed",
      );
    const code = codeStamp(runtimeDistTargets(layout));
    if (code !== context.actor.codeStamp)
      throw new HistoryConfigurationRefusal(
        "history child runtime code differs from recorded scope",
      );
    return {
      codeStamp: code,
      serviceSpecsDigest: context.actor.serviceSpecsDigest,
      incarnation: `${context.actor.attemptId}:${process.pid}`,
    };
  };
  const signed = makeHistorySignedAdmission({
    layout,
    run,
    expectedScope: {
      codeStamp: context.actor.codeStamp,
      serviceSpecsDigest: context.actor.serviceSpecsDigest,
      incarnation: `${context.actor.attemptId}:${context.actor.childPid}`,
    },
    currentScope,
    publicBindingDigest: context.specification.publicBindingDigest,
    deploymentFingerprint: context.actor.deploymentFingerprint,
    expectedNetwork: context.specification.expectedNetwork,
  });
  let announce: (error: HistoryConfigurationRefusal) => void = () => undefined;
  const refusal = new Promise<HistoryConfigurationRefusal>((resolve) => {
    announce = resolve;
  });
  const current = (deadline: number) => {
    try {
      return signed.current(deadline);
    } catch (error) {
      if (error instanceof HistoryConfigurationRefusal) announce(error);
      throw error;
    }
  };
  const admission = await retryHistoryAdmission(signed, signal);
  if (admission === undefined) return undefined;
  return {
    ...context,
    admission,
    current,
    refusal,
  };
};
export type AdmittedHistoryChild = NonNullable<
  Awaited<ReturnType<typeof admitHistoryChild>>
>;
