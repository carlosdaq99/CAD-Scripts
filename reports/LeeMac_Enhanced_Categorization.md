# **LeeMac AutoLISP Scripts: Enhanced Reference Catalog**
## Machine-Readable Professional Script Collection

### 📚 **OVERVIEW**

This enhanced reference provides both human-readable documentation and machine-readable metadata for 163 professional AutoLISP scripts in the LeeMac collection. The catalog is generated from structured YAML manifests enabling precise filtering, automated integration, and efficient context usage.

**Catalog Statistics:**
- **Total Scripts**: 163 AutoLISP programs
- **Categories**: 9 functional categories
- **Technologies**: 6 detected technologies
- **Complexity Levels**: 4 complexity classifications
- **Generated**: 2025-07-29 11:39:51

---

## 🔍 **QUICK LOOKUP TABLES**

### **By Category (9 categories)**
| Category | Count | Representative Scripts |
|----------|-------|----------------------|
| GEOMETRIC CURVE | 17 | `2DProjectionV1-0.lsp`, `AddLWPolylineVertexV1-1.lsp`, `AdvancedPolyOutlineV1-1.lsp` (+14 more) |
| MATHEMATICAL | 10 | `5PointEllipseV1-1.lsp`, `CircleBreakV1-3.lsp`, `CircleTangentsV1-0.lsp` (+7 more) |
| SPECIALIZED | 62 | `ACADDOCCreatorV1-1.lsp`, `AttModSuiteV1-1.lsp`, `Autoloader.lsp` (+59 more) |
| BLOCK ATTRIBUTE | 18 | `AddObjectsToBlockV1-2.lsp`, `Areas2AttributeV1-2.lsp`, `AttributeColour.lsp` (+15 more) |
| TEXT ANNOTATION | 32 | `AlignTextToCurveV1-5.lsp`, `AlignTextV1-4.lsp`, `AreaLabelV1-9.lsp` (+29 more) |
| SELECTION ENTITY | 8 | `BFindV2-0.lsp`, `ChainLengthV1-0.lsp`, `ChainSelV1-1.lsp` (+5 more) |
| USER INTERFACE | 4 | `BrowseForFolderV1-3.lsp`, `FormatDCLV1-0.lsp`, `ListBoxFunctionsV1-1.lsp` (+1 more) |
| LAYER DRAWING | 11 | `Copy2DrawingsV1-3.lsp`, `Copy2LayoutsV1-1.lsp`, `Copy2XRefV1-2.lsp` (+8 more) |
| IMPORT EXPORT | 1 | `DumpObjectV1-2.lsp` |


### **By Technology (6 technologies)**
| Technology | Count | Description |
|------------|-------|-------------|
| ActiveX | 123 | COM automation and object manipulation |
| SelectionSets | 73 | Entity selection and filtering |
| FileIO | 70 | File input/output operations |
| DCL | 42 | Dialog Control Language - Interactive user interfaces |
| VLR | 12 | Visual LISP Reactors - Event-driven programming |
| ObjectDBX | 13 | Multi-drawing batch operations without opening drawings |


### **By Complexity (4 levels)**
| Level | Count | Line Range | Typical Use Cases |
|-------|-------|------------|-------------------|
| Intermediate | 86 | 100-500 lines | Standard tools, moderate complexity |
| Advanced | 43 | 500-1500 lines | Professional tools, complex logic |
| Expert | 13 | > 1500 lines | Enterprise systems, comprehensive suites |
| Beginner | 21 | < 100 lines | Simple utilities, basic operations |


---

## 🚀 **COMMAND REFERENCE**

### **Direct Command Access**
| Command | Script | Category | Complexity |
|---------|---------|----------|------------|
| `2dpro` | `2DProjectionV1-0.lsp` | GEOMETRIC CURVE | intermediate |
| `AF` | `AreaLabelV1-9.lsp` | TEXT ANNOTATION | intermediate |
| `AT` | `AreaLabelV1-9.lsp` | TEXT ANNOTATION | intermediate |
| `AttCol` | `AttributeColour.lsp` | BLOCK ATTRIBUTE | intermediate |
| `AutoLoader` | `Autoloader.lsp` | SPECIALIZED | intermediate |
| `BFind` | `BFindV2-0.lsp` | SELECTION ENTITY | expert |
| `BlockCount` | `BlockCountV1-5.lsp` | BLOCK ATTRIBUTE | intermediate |
| `CurveText` | `CurveTextV1-4.lsp` | TEXT ANNOTATION | intermediate |
| `DInfo` | `DInfoV1-5.lsp` | SPECIALIZED | advanced |
| `DOff` | `DoubleOffsetV1-1.lsp` | GEOMETRIC CURVE | intermediate |
| `DTCurve` | `DTCurveV2-9.lsp` | TEXT ANNOTATION | expert |
| `DTRemove` | `DTCurveV2-9.lsp` | TEXT ANNOTATION | expert |
| `DisableLock` | `ObjectLock.lsp` | SPECIALIZED | advanced |
| `DoubleOffset` | `DoubleOffsetV1-1.lsp` | GEOMETRIC CURVE | intermediate |
| `DynOff` | `DynOffV2-1.lsp` | SPECIALIZED | advanced |
| `FormatDCL` | `FormatDCLV1-0.lsp` | USER INTERFACE | intermediate |
| `GetSyntax` | `GetSyntax.lsp` | SPECIALIZED | beginner |
| `IntLen` | `IntLenV1-4.lsp` | SPECIALIZED | intermediate |
| `IntLenM` | `IntLenV1-4.lsp` | SPECIALIZED | intermediate |
| `LX` | `LayerExtractV1-4.lsp` | LAYER DRAWING | expert |

*... and 195 more commands. See JSON catalog for complete list.*


---

## 🛠️ **TECHNOLOGY INTEGRATION**

### **Multi-Drawing Operations (ObjectDBX)**
Scripts using ObjectDBX for batch processing across multiple drawings:

- **BFindV2-0.lsp** - This could be accomplished programmatically, but has been omitted for better ;;...
  - Commands: `BFind`
- **Copy2DrawingsV1-3.lsp** - This program enables the user to copy a selection of objects to a   ;;...
  - Commands: `c2dwg`
- **Copy2XRefV1-2.lsp** - This program enables the user to copy a selection of objects to a   ;;...
  - Commands: `c2x`
- **CopyBlockDefinitionV1-1.lsp** - Returns the copied VLA Block Definition Object, else nil...
- **CopyRenameBlockV1-5.lsp** - This program allows a user to copy and/or rename a single block     ;;...
  - Commands: `cb`, `rb`
- **DeleteBlocksV1-1.lsp** - This program enables the user to quickly remove multiple blocks     ;;...
  - Commands: `delblocks`
- **ImportBlockV1-2.lsp** - This program allows a user to import a block from a selected        ;;...
  - Commands: `ib`
- **LayerExtractV1-4.lsp** - --=={ Local Functions }==--...
  - Commands: `LX`, `LayerExtract`
- **MacAttV3-1.lsp** - The function will retrieve specific attributes from blocks that appear in a  ;;...
  - Commands: `MacAttExt`, `MacAtt`, `MacAttEdit`
- **NestedMoveV1-2.lsp** - This program enables the user to move multiple selected objects     ;;...
  - Commands: `nmove`
- **ObjectDBXWrapperV1-2.lsp** - Evaluates a supplied function on all drawings in a given list or    ;;...
- **ResetXRefLayersV1-2.lsp** - This program enables the user to reset all or specific layer        ;;...
  - Commands: `rxl`
- **StealV1-8.lsp** - Program Overview                                       ;;...
  - Commands: `Steal`, `StealTemplate`, `StealAll`, `StealTemplates`, `StealLast`


### **Interactive Dialogs (DCL)**
Scripts providing interactive user interfaces:

- **AlignTextToCurveV1-5.lsp** - Program Overview                                                    ;;...
- **Areas2AttributeV1-2.lsp** - This program allows a user to populate a selected attribute with    ;;...
- **AttModSuiteV1-1.lsp** - Professional AutoLISP utility program...
- **AttributeColour.lsp** - Prompts for a selection of attributed blocks and displays ;;...
- **BatchAttributeEditorV1-5.lsp** - This program allows the user to modify the values of multiple attributes...


### **Event-Driven Programming (VLR)**
Scripts using Visual LISP Reactors for automatic behavior:

- **AssociativeCenterlineV1-0.lsp** - Professional AutoLISP utility program...
- **AssociativeTextboxV1-2.lsp** - Program Overview                                                    ;;...
- **AutoLabelAttributesV1-4.lsp** - This program will automatically populate a specific attribute tag   ;;...
- **BoundingBoxReactorV1-0.lsp** - Allows the user to create a new reactor owner group for   ;;...
- **ClockV1-1.lsp** - Professional AutoLISP utility program...
- **DTCurveV2-9.lsp** - The Program will prompt the user to either Select existing text to align,    ;;...
- **LayerDirectorV2-1.lsp** - AutoLISP utility program...
- **LayerDirectorV2-1.lsp** - AutoLISP utility program...
- **LISPLogV1-0.lsp** - Professional AutoLISP utility program...
- **MTEditReactorV1-1.lsp** - Professional AutoLISP utility program...
- **ObjectLock.lsp** - clearly isn't practical. The functionality can be         ;;...
- **SelectionCounterV1-4.lsp** - - Program modified to fix bug when using the UNDO command.          ;;...


---

## 📊 **USAGE STATISTICS**

### **Category Distribution**
- **SPECIALIZED**: 62 scripts (38.0%)
- **TEXT ANNOTATION**: 32 scripts (19.6%)
- **BLOCK ATTRIBUTE**: 18 scripts (11.0%)
- **GEOMETRIC CURVE**: 17 scripts (10.4%)
- **LAYER DRAWING**: 11 scripts (6.7%)
- **MATHEMATICAL**: 10 scripts (6.1%)
- **SELECTION ENTITY**: 8 scripts (4.9%)
- **USER INTERFACE**: 4 scripts (2.5%)
- **IMPORT EXPORT**: 1 scripts (0.6%)


### **Technology Adoption**
- **ActiveX**: 123 scripts (75.5%)
- **SelectionSets**: 73 scripts (44.8%)
- **FileIO**: 70 scripts (42.9%)
- **DCL**: 42 scripts (25.8%)
- **ObjectDBX**: 13 scripts (8.0%)
- **VLR**: 12 scripts (7.4%)


---

## 🔧 **DEVELOPER INTEGRATION**

### **JSON API Access**
For programmatic access, use the companion `leemac_catalog.json` file:

```python
import json

# Load catalog
with open('leemac_catalog.json', 'r') as f:
    catalog = json.load(f)

# Find scripts by technology
objectdbx_scripts = catalog['technologies']['ObjectDBX']

# Find scripts by category
text_scripts = catalog['categories']['TEXT_ANNOTATION']

# Get script details
for script in catalog['scripts']:
    if script['id'] in objectdbx_scripts:
        print(f"{script['filename']}: {script['commands']}")
```

### **MCP Tool Integration**
This catalog is optimized for Model Context Protocol (MCP) tools:

- **Precise Filtering**: Query by category, technology, or complexity
- **Context Optimization**: Minimal token usage for script lookup
- **Automated Integration**: Machine-readable metadata for tooling
- **Multi-dimensional Search**: Cross-category and tag-based filtering

### **Copilot Optimization**
Enhanced for GitHub Copilot workflows:

- **90% Context Reduction**: Condensed lookup tables vs full descriptions
- **Direct Command Access**: Immediate command-to-script mapping
- **Technology Tagging**: Filter by ObjectDBX, DCL, VLR, etc.
- **Complexity Awareness**: Choose appropriate scripts for task complexity

---

## 📝 **METADATA SCHEMA**

Each script includes structured metadata:

```yaml
id: "ScriptName"
filename: "ScriptName.lsp"
title: "Human Friendly Name"
description: "Detailed description"
primary_category: "CATEGORY_NAME"
commands: ["command1", "command2"]
tags: ["tag1", "tag2", "tag3"]
technologies: ["DCL", "ObjectDBX", "VLR"]
ui: "DCL" | "CommandLine" | "Graphics"
complexity: "beginner" | "intermediate" | "advanced" | "expert"
version_info: {"major": "1", "minor": "0"}
entry_points: {"primary": "command", "complexity": "intermediate"}
dependencies: {"requires": [], "integrates_with": [], "conflicts_with": []}
usage_context: {"typical_workflows": [], "automation_level": "semi_automatic"}
```

---

**📅 Generated**: 2025-07-29 11:39:51  
**📊 Scripts Cataloged**: 163 AutoLISP programs  
**🔬 Methodology**: YAML manifest aggregation with automated indexing  
**⭐ Optimization**: Context-efficient, machine-readable, MCP-compatible  
**🎯 Target Audience**: AI assistants, AutoLISP developers, CAD automation professionals  

---

*This enhanced catalog provides both human-readable reference and machine-queryable metadata, optimizing context window usage while enabling precise script discovery and automated integration workflows.*
