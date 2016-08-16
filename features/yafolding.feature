Feature: Fold all lines
  In order to be able to visualize the outline of my file
  As a user
  I want to be able to fold all elements at once

  Scenario: Empty buffers are not changed
    Given I have an empty buffer
    When I call yafolding-hide-all
    Then I should have an empty buffer
