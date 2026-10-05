import {
  applyVerifiedL1Recovery,
  consumeL1RecoveryCertificate,
  type L1RecoveryCertificate,
  type L1RecoverySnapshot,
  l1RecoverySnapshot,
  readL1RecoveryCertificate,
} from "../l1/recovery-incident.js";
import type { StoreData } from "../store.committee-store.js";
import type { CommitteeRetirementController } from "./retirement-model.js";

export const jsonL1RecoverySnapshot =
  (
    join: () => Promise<void>,
    assertHeld: () => Promise<void>,
    read: () => Promise<StoreData>,
  ) =>
  async (): Promise<L1RecoverySnapshot> => {
    await join();
    await assertHeld();
    return l1RecoverySnapshot(await read());
  };

export const jsonApplyL1Recovery =
  (
    retirement: CommitteeRetirementController,
    read: () => Promise<StoreData>,
    write: (data: StoreData) => Promise<void>,
    assertHeld: () => Promise<void>,
    closed: () => boolean,
    schedule: (run: () => Promise<void>) => Promise<void>,
  ) =>
  async (certificate: L1RecoveryCertificate): Promise<void> => {
    if (closed()) throw new Error("committee node file store is closed");
    const guard = retirement.capture();
    try {
      await schedule(async () => {
        await assertHeld();
        const data = await read();
        retirement.assert(guard);
        const verified = readL1RecoveryCertificate(certificate);
        applyVerifiedL1Recovery(data, certificate);
        await verified.assertCurrent();
        retirement.assert(guard);
        const next = applyVerifiedL1Recovery(data, certificate);
        await assertHeld();
        verified.assertScopeCurrent();
        await write(next);
      });
    } finally {
      consumeL1RecoveryCertificate(certificate);
    }
  };
