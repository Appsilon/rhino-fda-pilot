describe('Filter panes in KM tab', () => {
  // Rhino applications have an 'app' prefix
  const nsPrefix = 'app-';
  // Support function to help generating namespaces
  const ns = (id) => `#${nsPrefix}${id}`;
  // Support function to help generating teal namespaces on main UI
  const nsTeal = (id) => ns(`teal_wrapper-teal-main_ui-${id}`);

  afterEach(() => {
    // Make sure that html element does not have a class that indicates
    // that shiny is busy
    cy.get('html').not('.shiny-busy');

    cy.get('@active_tab').within(() => {
      cy.get('.shiny-bound-output:visible').each(($el) => {
        cy.wrap($el).children().should('have.length.gte', 1);
      });
    });
  });

  beforeEach(() => {
    cy.visit('/');

    cy.get('html').not('.shiny-busy');
    // Look for 'Data is loaded' element
    cy.contains('Data loaded - App fully started up');

    // Find the tab and click to navigate
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]')
      .contains('KM plot for TTDE')
      .as('kmNav');

    cy.get('@kmNav').click();

    // Make sure that html element does not have a class that indicates
    // that shiny is busy
    cy.get('html').not('.shiny-busy');

    cy.get('@kmNav').invoke('attr', 'href').then((hrefTab) => {
      // Define an alias that references the current active tab
      cy
        .get(`${hrefTab}.tab-pane.active`)
        .should('be.visible')
        .as('active_tab');
    });

    // Define an alias that references the active filters
    cy.get(nsTeal('filter_panel-filters_overview')).as('filter_summary');

    // Define an alias that references the active variables
    cy.get(nsTeal('filter_panel-filter_active_vars')).as('filter_variables');

    // Define an alias that references the section that adds new variables to
    //  the filters
    cy.get(nsTeal('filter_panel-filter_add_vars')).as('add_filter');
    cy.get('@add_filter').should('be.visible');

    // Make sure that html element does not have a class that indicates
    // that shiny is busy
    cy.get('html').not('.shiny-busy');

  });

  // Hamburger works hidding / showing
  // ######################################

  it('Hamburger menu shows by default and hides/shows when clicked', () => {
    cy.get('#filter_hamburger').as('hamburger-menu');

    // By default, the filter pane should be visible
    cy.get('@filter_summary').should('be.visible');

    // Hides the filter pane when clicking the button
    cy.get('@hamburger-menu').click();
    cy.get('@filter_summary').should('not.be.visible');

    // Shows the filter pane when clicking the button again
    cy.get('@hamburger-menu').click();
    cy.get('@filter_summary').should('be.visible');
  });

  // Alert is dismissible
  it('Alert in KM tab is dismissible', () => {
    cy.get('@active_tab').within(() => {
      cy.get('.alert.alert-dismissible').as('alert');

      cy.get('@alert').should('be.visible');
      cy.get('@alert').get('button.close').click();
      cy.get('@alert').should('not.exist');
    });
  });

  //  Filter summary
  // ######################################

  it('Filter Summary has non-zero Observations', () => {
    cy.get('@filter_summary').within(() => {
      cy.get('.shiny-bound-output').each(($el) => {
        cy.wrap($el).children().should('have.length.gte', 1);
      });
    });
  });

  //  Filter variables
  // ######################################

  it('Filter variables has shiny content rendered', () => {
    cy.get('@filter_variables').within(() => {
      cy
        .get(nsTeal('filter_panel-filter_active_vars_contents'))
        .should('have.length.gte', 1);

      cy
        .get(nsTeal('filter_panel-ADSL_filter-filter-cards'))
        .children()
        .should('have.length', 0);

      cy
        .get(nsTeal('filter_panel-ADAS_filter-filter-cards'))
        .children()
        .should('have.length', 0);

      cy
        .get(nsTeal('filter_panel-ADTTE_filter-filter-cards'))
        .children()
        .should('have.length', 0);

      cy
        .get(nsTeal('filter_panel-ADLB_filter-filter-cards'))
        .children()
        .should('have.length', 0);
    });
  });

  //  Add variables to filter
  // ######################################

  it('Add filter variables has shiny content rendered', {
    defaultCommandTimeout: 10000,
  }, () => {
    cy.get('@add_filter').within(() => {
      cy.get('.shiny-input-container').each(($el) => {
        cy.wrap($el).children().should('have.length.gte', 1);
      });
    });
  });

  //  Add variables to filter
  // ######################################

  it('Add filter', () => {
    const nsInfoTable = (selector) => `${nsTeal('filter_panel-teal_filters_info-table')} ${selector}`;

    // Add Age filter (find and click on it)
    // ------------------------------------
    cy
      .get('@add_filter')
      .find('.shiny-input-container:first')
      .contains('Select variable to filter')
      .should('be.visible')
      .parent();

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    cy
      .get('@add_filter')
      .find('.shiny-input-container:first', { timeout: 15000 })
      .parent()
      .click('top');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    cy
      .get('@add_filter')
      .find('.dropdown-menu.open li')
      .contains('Age')
      .as('age');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    cy.get('@age', { timeout: 15000 }).click('top');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    // Test if application has non-zero subjects

    cy
      .get(nsInfoTable('table tbody tr:first td:last'))
      .contains(/[1-9]+[0-9]*\/[1-9]+[0-9]*/);

    // Monitor how many times summary is recalculated vias shiny:recalculated
    // event
    cy.get('@filter_summary').within(() => {
      cy
        .get('.shiny-bound-output')
        .invoke('on', 'shiny:recalculated', cy.stub().as('summary_change'));
    });

    // Move slider via cypress2 mouse drag
    // ------------------------------------
    cy.get('@filter_variables').within(() => {
      // Make sure that the overlay plot is rendered
      cy
        .get('.filterPlotOverlayRange .shiny-plot-output')
        .children()
        .should('have.length.gte', 1);

      cy
        .get('.filterPlotOverlayRange .shiny-plot-output')
        .not('.recalculating');

      // Move the handle 3x
      //  note: this is necessary as developer wasn't able to define a single
      //    long drag. This is open to improvement
      cy.get('.irs-handle.from').as('irs-handle');
    });

    const mousedownOpts = {
      button: 0, which: 1, pageX: 600, pageY: 100,
    };
    const mousemoveOpts = {
      clientX: 300, clientY: 400,
    };

    cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
    cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
    cy.get('@irs-handle').trigger('mouseup', 'top');

    cy
      .get('@filter_variables')
      .find('.filterPlotOverlayRange .shiny-plot-output')
      .not('.recalculating');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
    cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
    cy.get('@irs-handle').trigger('mouseup', 'top');

    cy
      .get('@filter_variables')
      .find('.filterPlotOverlayRange .shiny-plot-output')
      .not('.recalculating');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    cy.get('@irs-handle').trigger('mousedown', 'top', mousedownOpts);
    cy.get('@irs-handle').trigger('mousemove', 'top', mousemoveOpts);
    cy.get('@irs-handle').trigger('mouseup', 'top');

    cy
      .get('@filter_variables')
      .find('.filterPlotOverlayRange .shiny-plot-output')
      .not('.recalculating');

    // Let shiny finish rendering
    cy.get('html').not('.shiny-busy');

    // Verify that data is filtered (selected subjets != dataset)
    // ------------------------------------
    cy
      .get(nsInfoTable('table tbody tr:first td:last'), { timeout: 10000 })
      .should('satisfy', ($el) => {
        const result = /([0-9]+)\/([0-9]+)/.exec($el[0].innerText);
        return result.length === 3 && result[1] !== result[2];
      });
  });
});
