import "./field-carriage-prerequisite.production-field-carriage-prerequisite-v1.js";

import { describe, expect, it, vi } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { withFieldCarriagePrerequisite } from "../src/workflow/field-carriage-prerequisite.js";
import {
  base,
  baseAction,
  context,
  prerequisite,
  publicationAction,
} from "./field-carriage-prerequisite.prerequisite.js";

describe("fresh prerequisite selection changes", () => {
  it.each([
    "base_pending",
    "publication_changed",
    "required_again",
    "pending_again",
  ] as const)(
    "yields %s before capturing or submitting stale work",
    async (change) => {
      const underlying = base();
      const port = prerequisite();
      const adapter = withFieldCarriagePrerequisite({
        category: "nonExistentInput",
        base: underlying,
        prerequisite: port,
      });
      const action =
        change === "required_again" || change === "pending_again"
          ? baseAction
          : publicationAction;
      vi.mocked(port.inspect).mockResolvedValueOnce(
        action === baseAction
          ? { kind: "satisfied" }
          : { kind: "required", action: publicationAction },
      );
      await expect(adapter.observe(context)).resolves.toMatchObject({
        kind: "action_required",
        action,
      });
      if (change === "base_pending")
        vi.mocked(underlying.observe).mockResolvedValue({
          kind: "pending",
          reason: "canonical thread changing",
        });
      else if (change === "publication_changed")
        vi.mocked(port.inspect).mockResolvedValue({
          kind: "required",
          action: { ...publicationAction, actionId: "replacement-publication" },
        });
      else if (change === "pending_again")
        vi.mocked(port.inspect).mockResolvedValue({
          kind: "pending",
          reason: "publication inclusion being reconciled",
        });
      await expect(
        adapter.preflight({ ...context, action }),
      ).rejects.toBeInstanceOf(WorkflowActionChangedError);
      expect(port.capture).not.toHaveBeenCalled();
      expect(underlying.preflight).not.toHaveBeenCalled();
      expect(underlying.submit).not.toHaveBeenCalled();
    },
  );

  it("preserves prerequisite integrity failures as hard errors", async () => {
    const underlying = base();
    const port = prerequisite();
    const integrity = new Error(
      "authenticated publication datum was substituted",
    );
    vi.mocked(port.inspect).mockRejectedValue(integrity);
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toBe(integrity);
    expect(integrity).not.toBeInstanceOf(WorkflowActionChangedError);
    expect(port.capture).not.toHaveBeenCalled();
    expect(underlying.preflight).not.toHaveBeenCalled();
  });
});
