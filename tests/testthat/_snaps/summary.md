# test cardSummary

    Code
      displayOutput(x)
    Output
      <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
        <div class="card-header">Results summary</div>
        <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
          <h3>Result overview</h3>
      <ul>
      <li>Results contain <strong>0</strong> rows with <strong>0</strong> different result_ids.</li>
      <li>Results contain <strong>0</strong> different result types.</li>
      <li>Results contain data from <strong>0</strong> different cdm objects.</li>
      </ul>
      <h3>Package versions</h3>
      <h3>Result suppression</h3>
      <h3>Explore settings</h3>
      
          <div class="datatables html-widget html-fill-item" id="htmlwidget-54fc7ec149945f3f3c9a" style="width:100%;height:auto;"></div>
          <script type="application/json" data-for="htmlwidget-54fc7ec149945f3f3c9a">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\" disabled=\"\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"0\" data-max=\"1\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\" disabled=\"\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\" disabled=\"\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\" disabled=\"\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[[],[],[],[]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>result_id<\/th>\n      <th>result_type<\/th>\n      <th>package_name<\/th>\n      <th>package_version<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"scrollX":true,"columnDefs":[{"className":"dt-right","targets":0},{"name":"result_id","targets":0},{"name":"result_type","targets":1},{"name":"package_name","targets":2},{"name":"package_version","targets":3}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}</script>
        </div>
        <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
      </div>

---

    Code
      displayOutput(x)
    Output
      <div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-require-bs-caller="card()" data-require-bs-version="5">
        <div class="card-header">Results summary</div>
        <div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
          <h3>Result overview</h3>
      <ul>
      <li>Results contain <strong>3</strong> rows with <strong>3</strong> different result_ids.</li>
      <li>Results contain <strong>2</strong> different result types: <code>counts</code> and <code>sums</code>.</li>
      <li>Results contain data from <strong>2</strong> different cdm objects: &quot;<em>mock</em>&quot; and &quot;<em>eunomia</em>&quot;.</li>
      </ul>
      <h3>Package versions</h3>
      <p>Inconsistent versions:</p>
      <ul>
      <li><span style="color:red"> <strong>OmopViewer</strong> 0.1.0 in result_id(s): 1</span></li>
      <li><span style="color:red"> <strong>OmopViewer</strong> 0.2.0 in result_id(s): 2</span></li>
      </ul>
      <p>Consistent versions:</p>
      <ul>
      <li><span style="color:green"> <strong>omopgenerics</strong> 1.0.0 in result_id(s): 3</span></li>
      </ul>
      <h3>Result suppression</h3>
      <ul>
      <li><span style="color:red"> <strong>2</strong> not suppressed results.</span></li>
      <li><span style="color:green"> <strong>1</strong> suppressed results at minCellCount = <code>5</code>.</span></li>
      </ul>
      <h3>Explore settings</h3>
      
          <div class="datatables html-widget html-fill-item" id="htmlwidget-56c06f4b75099cd2bc2f" style="width:100%;height:auto;"></div>
          <script type="application/json" data-for="htmlwidget-56c06f4b75099cd2bc2f">{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"integer\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"3\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"1\" data-max=\"5\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[[1,2,3],["counts","counts","sums"],[null,1,5],["OmopViewer","OmopViewer","omopgenerics"],["0.1.0","0.2.0","1.0.0"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>result_id<\/th>\n      <th>result_type<\/th>\n      <th>min_cell_count<\/th>\n      <th>package_name<\/th>\n      <th>package_version<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"scrollX":true,"columnDefs":[{"className":"dt-right","targets":[0,2]},{"name":"result_id","targets":0},{"name":"result_type","targets":1},{"name":"min_cell_count","targets":2},{"name":"package_name","targets":3},{"name":"package_version","targets":4}],"order":[],"autoWidth":false,"orderClasses":false,"orderCellsTop":true},"selection":{"mode":"multiple","selected":null,"target":"row","selectable":null}},"evals":[],"jsHooks":[]}</script>
        </div>
        <script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
      </div>

