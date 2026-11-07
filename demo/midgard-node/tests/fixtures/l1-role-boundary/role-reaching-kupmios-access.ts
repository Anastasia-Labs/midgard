/**
 * A role entry that statically reaches the Kupmios adapter: the boundary
 * check must flag it (`tests/l1-role-boundary.test.ts`).
 */
import { openKupmiosAccess } from "../../../src/l1-external/kupmios-access.js";

export const roleAccess = openKupmiosAccess;
