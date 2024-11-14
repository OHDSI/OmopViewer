# background

    Code
      cat(createBackground(TRUE), sep = "\n")
    Output
      bslib::nav_panel(
          title = "Background",
          icon = shiny::icon("disease"),
          OmopViewer::cardFromMd("background.md")
        )

---

    Code
      cat(createBackground(FALSE), sep = "\n")
    Output
      

# test cardFromMd

    Code
      cat(as.character(bkg))
    Output
      <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
        <div class="card-header">This is the header of the background</div>
        <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;"><h1 id="title">Title</h1>
      <h2 id="subtitle">subtitle</h2>
      <p>content</p>
      </div>
        <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
      </div>

