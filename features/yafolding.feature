Feature: Fold all lines
  In order to be able to visualize the outline of my file
  As a user
  I want to be able to fold all elements at once

  Scenario: Empty buffers are not changed
    Given I have an empty buffer
    When I call yafolding-hide-all
    Then I should have an empty buffer

  Scenario: Test file from issue #23 should collapse correctly
    Given I have a buffer with "test/data/issue-23.txt"
    And I am on line 2
    When I call yafolding-hide-all
    And I go to line 1
    And I call yafolding-hide-element
    Then I should see only 1 line

  Scenario: Elements after empty lines should collapse correctly
    Given I have a buffer with "test/data/issue-29.txt"
    And I am on line 3
    When I call yafolding-hide-all
    Then I should see 5 lines
