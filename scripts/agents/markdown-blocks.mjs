// Splits agent-facing Markdown into the blocks the agent checks reason about:
// list items and paragraphs, each with its first line number and the heading
// it sits under (`section`, at `sectionLevel`; 0 before any heading). Front
// matter, fenced code, tables and HTML comments are not prose and never form a
// block, but the comments a block carries are kept in `comments` so a marker
// such as `<!-- doc-links:future -->` can apply to it.

const fence = /^\s*(`{3,}|~{3,})/u;
const listItem = /^\s*(?:[-*+]|\d+[.)])\s+/u;
const heading = /^\s{0,3}#{1,6}\s/u;

// Inline code spans, removed before looking for words or tags in prose.
export const withoutInlineCode = (text) => text.replace(/(`+)[^`]*?\1/gu, " ");

export const markdownBlocks = (source) => {
  const lines = source.split(/\r?\n/u);
  const blocks = [];
  let current;
  let section = "";
  let sectionLevel = 0;
  let fenceMarker;
  let inComment = false;
  let index = 0;

  const close = () => {
    if (current !== undefined) blocks.push(current);
    current = undefined;
  };

  if (lines[0] === "---") {
    const end = lines.indexOf("---", 1);
    if (end > 0) index = end + 1;
  }

  for (; index < lines.length; index += 1) {
    const raw = lines[index];
    const lineNumber = index + 1;

    if (fenceMarker !== undefined) {
      if (raw.trim().startsWith(fenceMarker)) fenceMarker = undefined;
      continue;
    }
    const opened = fence.exec(raw);
    if (opened) {
      close();
      fenceMarker = opened[1][0].repeat(3);
      continue;
    }

    // HTML comments: kept as block annotations, removed from the text.
    let line = raw;
    const comments = [];
    if (inComment) {
      const end = line.indexOf("-->");
      if (end < 0) {
        comments.push(line);
        continue;
      }
      comments.push(line.slice(0, end));
      line = line.slice(end + 3);
      inComment = false;
    }
    line = line.replace(/<!--([\s\S]*?)-->/gu, (_match, body) => {
      comments.push(body);
      return "";
    });
    const openComment = line.indexOf("<!--");
    if (openComment >= 0) {
      comments.push(line.slice(openComment + 4));
      line = line.slice(0, openComment);
      inComment = true;
    }

    if (line.trim() === "") {
      if (comments.length > 0) {
        // A comment on its own line annotates the block that follows it.
        close();
        current = {
          line: lineNumber,
          section,
          sectionLevel,
          text: "",
          comments,
        };
        continue;
      }
      if (current !== undefined && current.text !== "") close();
      continue;
    }
    if (heading.test(line)) {
      close();
      sectionLevel = /#+/u.exec(line)[0].length;
      section = line.replace(/^\s*#+\s*/u, "").trim();
      continue;
    }
    if (line.trim().startsWith("|")) {
      close();
      continue;
    }
    if (listItem.test(line) || current === undefined || current.text === "") {
      const pending = current?.text === "" ? current.comments : [];
      if (current?.text !== "") close();
      current = {
        line: lineNumber,
        section,
        sectionLevel,
        text: line.trim(),
        comments: [...pending, ...comments],
      };
      continue;
    }
    current.text += ` ${line.trim()}`;
    current.comments.push(...comments);
  }
  close();
  return blocks.filter((block) => block.text !== "");
};
