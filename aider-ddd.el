;;; aider-ddd.el --- Domain-Driven Design operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides Domain-Driven Design (DDD) functionality for the Aider package.
;; It helps apply DDD principles to codebases with AI assistance.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

(defcustom aider-ddd-ubiquitous-language-file "ubiquitous-language.md"
  "File name for storing the ubiquitous language glossary."
  :type 'string
  :group 'aider)

;;;###autoload
(defun aider-ddd-analyze-domain-model ()
  "Analyze current codebase to identify domain concepts and relationships.
Uses AI to extract implicit domain model from code."
  (interactive)
  (let* ((initial-input "Analyze this code from a Domain-Driven Design perspective. Identify:")
         (candidate-list '("Identify key domain entities, value objects, and aggregates in this code"
                          "Map out the domain relationships and boundaries in this code"
                          "Identify implicit domain concepts that should be made explicit"
                          "Analyze this code for bounded contexts and suggest context boundaries"
                          "Identify domain events and commands in this system"
                          "Suggest a more domain-aligned structure for this code"))
         (instruction (aider-read-string "Domain model analysis: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-refactor-to-entity ()
  "Refactor selected class or code into a proper DDD entity.
Ensures the class has identity, lifecycle, and domain behavior."
  (interactive)
  (let* ((class-name (or (aider--get-class-at-point)
                        (read-string "Entity class name: ")))
         (initial-input (format "Refactor class '%s' into a proper DDD entity with:" class-name))
         (candidate-list '("Clear identity concept and equality based on identity"
                          "Proper encapsulation of state with domain-meaningful methods"
                          "Validation rules that enforce domain invariants"
                          "Rich domain behavior rather than anemic getters/setters"
                          "Lifecycle management appropriate for this entity"))
         (instruction (aider-read-string "Entity refactoring: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-refactor-to-value-object ()
  "Refactor selected class or code into a proper DDD value object.
Ensures immutability, equality by value, and self-validation."
  (interactive)
  (let* ((class-name (or (aider--get-class-at-point)
                        (read-string "Value object class name: ")))
         (initial-input (format "Refactor class '%s' into a proper DDD value object with:" class-name))
         (candidate-list '("Complete immutability (no setters or state changes after creation)"
                          "Equality based on all attributes (value equality)"
                          "Self-validation in constructor"
                          "Domain-meaningful methods that return new instances"
                          "No identity concept or lifecycle"))
         (instruction (aider-read-string "Value object refactoring: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-create-aggregate ()
  "Create or refactor code into a proper DDD aggregate.
Defines aggregate root, consistency boundaries, and access rules."
  (interactive)
  (let* ((aggregate-name (read-string "Aggregate root name: "))
         (initial-input (format "Create or refactor code into a proper DDD aggregate with '%s' as the root:" aggregate-name))
         (candidate-list '("Clear aggregate boundaries and consistency rules"
                          "Proper encapsulation of child entities and value objects"
                          "Transactional consistency guarantees within the aggregate"
                          "Access to internal entities only through the aggregate root"
                          "Domain events for significant state changes"))
         (instruction (aider-read-string "Aggregate creation: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-design-repository ()
  "Design a repository for a domain entity or aggregate.
Creates interface and implementation with proper persistence abstraction."
  (interactive)
  (let* ((entity-name (or (aider--get-class-at-point)
                         (read-string "Entity/Aggregate name: ")))
         (initial-input (format "Design a DDD repository for '%s' with:" entity-name))
         (candidate-list '("Collection-like interface (add/remove/find methods)"
                          "Persistence abstraction that hides storage details"
                          "Query methods that use domain-specific criteria"
                          "Proper transaction and concurrency handling"
                          "Implementation that works with the current infrastructure"))
         (instruction (aider-read-string "Repository design: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-identify-bounded-contexts ()
  "Analyze codebase to identify bounded contexts and context boundaries.
Suggests context mapping strategies between identified contexts."
  (interactive)
  (let* ((initial-input "Analyze this codebase to identify bounded contexts:")
         (candidate-list '("Identify separate bounded contexts based on language and concept usage"
                          "Suggest context boundaries and integration patterns between contexts"
                          "Identify shared kernels and potential anti-corruption layers"
                          "Map customer/supplier and conformist relationships between contexts"
                          "Suggest a strategic domain chart for this system"))
         (instruction (aider-read-string "Bounded context analysis: " initial-input candidate-list)))
    (aider--send-command (concat "/ask " instruction) t)))

;;;###autoload
(defun aider-ddd-extract-domain-service ()
  "Extract domain logic into a proper domain service.
Identifies and moves domain operations that don't belong in entities."
  (interactive)
  (let* ((service-name (read-string "Domain service name: "))
         (initial-input (format "Extract domain logic into a '%s' domain service:" service-name))
         (candidate-list '("Operations that involve multiple entities or aggregates"
                          "Complex domain rules that don't belong to a single entity"
                          "Stateless operations that represent domain processes"
                          "Coordination of domain workflows and transactions"
                          "External system integration that has domain meaning"))
         (instruction (aider-read-string "Domain service extraction: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-design-domain-event ()
  "Design domain events for significant state changes.
Creates event classes and handling mechanisms."
  (interactive)
  (let* ((event-name (read-string "Domain event name: "))
         (initial-input (format "Design a '%s' domain event and its handling:" event-name))
         (candidate-list '("Event class with all relevant state information"
                          "Event publishing mechanism from the source aggregate"
                          "Event handlers for affected bounded contexts"
                          "Event persistence for audit and replay capabilities"
                          "Integration with existing event infrastructure"))
         (instruction (aider-read-string "Domain event design: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-refactor-to-specification ()
  "Create or refactor code into a DDD specification pattern.
Encapsulates complex domain rules and queries."
  (interactive)
  (let* ((spec-name (read-string "Specification name: "))
         (initial-input (format "Create a '%s' specification that encapsulates:" spec-name))
         (candidate-list '("Complex domain rules or criteria as a first-class concept"
                          "Composable specifications that can be combined with AND/OR logic"
                          "Both in-memory and persistence-specific implementations"
                          "Reusable validation or selection logic across the domain"
                          "Integration with repositories for efficient querying"))
         (instruction (aider-read-string "Specification design: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

;;;###autoload
(defun aider-ddd-manage-ubiquitous-language ()
  "Manage the ubiquitous language glossary for the project.
Creates, updates, or reviews the shared language definitions."
  (interactive)
  (let* ((glossary-file (expand-file-name aider-ddd-ubiquitous-language-file
                                         (or (aider--get-repo-root) default-directory)))
         (file-exists (file-exists-p glossary-file))
         (action (if file-exists
                    (completing-read "Ubiquitous language action: "
                                    '("Update glossary with new terms"
                                      "Review and refine existing terms"
                                      "Check code alignment with glossary"
                                      "Add current file terms to glossary")
                                    nil t)
                  "Create new ubiquitous language glossary"))
         (instruction (if (string= action "Create new ubiquitous language glossary")
                         (format "Create a ubiquitous language glossary for this project. Extract domain terms and their definitions from the code. Format as a markdown document with terms as headers and save to %s"
                                aider-ddd-ubiquitous-language-file)
                       (format "%s in %s. %s"
                              action
                              aider-ddd-ubiquitous-language-file
                              (cond
                               ((string= action "Update glossary with new terms")
                                "Identify new domain terms in the code that aren't in the glossary yet.")
                               ((string= action "Review and refine existing terms")
                                "Ensure definitions are precise and reflect current domain understanding.")
                               ((string= action "Check code alignment with glossary")
                                "Identify inconsistencies between code terminology and glossary terms.")
                               ((string= action "Add current file terms to glossary")
                                "Extract domain terms from the current file and add them to the glossary."))))))
    (if file-exists (aider-add-current-file))
    (aider--send-command (concat "/architect " instruction) t)))

;;;###autoload
(defun aider-ddd-strategic-design-workshop ()
  "Conduct a strategic design workshop with AI assistance.
Explores core domain, supporting domains, and strategic value."
  (interactive)
  (let* ((initial-input "Conduct a DDD strategic design workshop for this codebase:")
         (candidate-list '("Identify core domain vs supporting and generic subdomains"
                          "Map the current domain landscape and strategic importance"
                          "Suggest areas for strategic investment vs outsourcing"
                          "Identify big-picture events and workflows across the system"
                          "Create a domain vision statement and strategic goals"))
         (instruction (aider-read-string "Strategic design workshop: " initial-input candidate-list)))
    (aider--send-command (concat "/architect " instruction) t)))

;;;###autoload
(defun aider-ddd-tactical-patterns-review ()
  "Review code for proper application of DDD tactical patterns.
Identifies misused patterns and suggests improvements."
  (interactive)
  (let* ((initial-input "Review this code for proper application of DDD tactical patterns:")
         (candidate-list '("Identify entities that should be value objects or vice versa"
                          "Check for proper aggregate boundaries and consistency rules"
                          "Review repositories for proper abstraction and query methods"
                          "Identify domain services that contain entity behavior or vice versa"
                          "Check for anemic domain models and suggest behavior enrichment"))
         (instruction (aider-read-string "Tactical patterns review: " initial-input candidate-list)))
    (aider-current-file-command-and-switch "/architect " instruction)))

(provide 'aider-ddd)

;;; aider-ddd.el ends here
