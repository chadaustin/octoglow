const { invoke } = window.__TAURI__.core;

const tree = document.querySelector("#tree");
const selectionList = document.querySelector("#selection");
const statusEl = document.querySelector("#status");
const saveButton = document.querySelector("#save");
const cancelButton = document.querySelector("#cancel");
const selected = new Set();

function setStatus(message) {
  statusEl.textContent = message;
}

function renderSelection() {
  selectionList.replaceChildren();
  for (const path of [...selected].sort((a, b) => a.localeCompare(b))) {
    const item = document.createElement("li");
    item.title = path;
    item.textContent = path;
    selectionList.append(item);
  }
}

function makeGroup() {
  const group = document.createElement("div");
  group.className = "tree-group";
  group.setAttribute("role", "group");
  return group;
}

function makeNode(node) {
  const container = document.createElement("div");
  container.className = "tree-node";
  container.dataset.path = node.path;

  const row = document.createElement("div");
  row.className = "tree-row";
  row.setAttribute("role", "treeitem");
  row.setAttribute("aria-expanded", "false");

  const twisty = document.createElement("button");
  twisty.className = node.has_children ? "twisty" : "twisty empty";
  twisty.type = "button";
  twisty.tabIndex = -1;

  const checkbox = document.createElement("input");
  checkbox.type = "checkbox";
  checkbox.checked = selected.has(node.path);

  const label = document.createElement("span");
  label.className = "label";
  label.title = node.path;
  label.textContent = node.name;

  row.append(twisty, checkbox, label);
  container.append(row);

  checkbox.addEventListener("change", () => {
    if (checkbox.checked) {
      selected.add(node.path);
    } else {
      selected.delete(node.path);
    }
    renderSelection();
  });

  const expand = async () => {
    if (!node.has_children || container.dataset.loaded === "true") {
      const childGroup = container.querySelector(":scope > .tree-group");
      if (childGroup) {
        const isHidden = childGroup.hidden;
        childGroup.hidden = !isHidden;
        twisty.classList.toggle("expanded", isHidden);
        row.setAttribute("aria-expanded", String(isHidden));
      }
      return;
    }

    setStatus(`Loading ${node.path}`);
    try {
      const children = await invoke("list_children", { path: node.path });
      const group = makeGroup();
      for (const child of children) {
        group.append(makeNode(child));
      }
      container.append(group);
      container.dataset.loaded = "true";
      twisty.classList.add("expanded");
      row.setAttribute("aria-expanded", "true");
      setStatus("");
    } catch (error) {
      setStatus(String(error));
    }
  };

  twisty.addEventListener("click", expand);
  label.addEventListener("dblclick", expand);

  return container;
}

async function boot() {
  try {
    const saved = await invoke("load_selection");
    for (const path of saved) {
      selected.add(path);
    }
    renderSelection();

    const roots = await invoke("list_roots");
    const group = makeGroup();
    for (const root of roots) {
      group.append(makeNode(root));
    }
    tree.replaceChildren(group);
  } catch (error) {
    setStatus(String(error));
  }
}

saveButton.addEventListener("click", async () => {
  try {
    await invoke("save_selection", { paths: [...selected] });
    await invoke("close_window");
  } catch (error) {
    setStatus(String(error));
  }
});

cancelButton.addEventListener("click", async () => {
  try {
    await invoke("close_window");
  } catch (error) {
    setStatus(String(error));
  }
});

boot();
