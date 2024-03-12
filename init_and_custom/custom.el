;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "Guanghao Jiao")           ; User full name
;; (setq centaur-mail-address "guanghao.jiao.de@gmail.com")   ; Email address
;; (setq centaur-proxy "127.0.0.1:7890")          ; HTTP/HTTPS proxy
;; (setq centaur-socks-proxy "127.0.0.1:7890")    ; SOCKS proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon nil)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'netease)         ; Package repo: melpa, emacs-cn, bfsu, netease, sjtu, tencent, tuna or ustc
(setq centaur-theme 'doom-flatwhite)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
;; (setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
;; (setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
;; (setq centaur-tree-sitter nil)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
;; (setq centaur-chinese-calendar t)              ; Support Chinese calendar or not: t or nil
;; (setq centaur-player t)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)
;; (proxy-socks-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" default))
 '(org-agenda-files
   '("c:/Users/j8154/AppData/Roaming/org/20240115103653-index.org" "c:/Users/j8154/AppData/Roaming/org/20240115103654-nuclear_reactor.org" "c:/Users/j8154/AppData/Roaming/org/20240115110040-kernenergie_basis_wissen.org" "c:/Users/j8154/AppData/Roaming/org/20240115141649-motivation_of_two_phase_flow_in_sfr.org" "c:/Users/j8154/AppData/Roaming/org/20240122083444-stress.org" "c:/Users/j8154/AppData/Roaming/org/20240122231030-dot_product.org" "c:/Users/j8154/AppData/Roaming/org/20240124002604-impulsgleichung_diff_form.org" "c:/Users/j8154/AppData/Roaming/org/20240124104140-basic_knowledge.org" "c:/Users/j8154/AppData/Roaming/org/20240124104222-nuclear_reactor.org" "c:/Users/j8154/AppData/Roaming/org/20240124104254-fluid_mechanics.org" "c:/Users/j8154/AppData/Roaming/org/20240124104425-linear_algebra.org" "c:/Users/j8154/AppData/Roaming/org/20240124104447-x_ray_imaging.org" "c:/Users/j8154/AppData/Roaming/org/20240124105358-navier_stokes_gleichungen.org" "c:/Users/j8154/AppData/Roaming/org/20240124115745-fluid_element.org" "c:/Users/j8154/AppData/Roaming/org/20240124120205-feldgroße.org" "c:/Users/j8154/AppData/Roaming/org/20240124120718-x_ray_tomography.org" "c:/Users/j8154/AppData/Roaming/org/20240124121715-tensor.org" "c:/Users/j8154/AppData/Roaming/org/20240124144726-nabla_operator.org" "c:/Users/j8154/AppData/Roaming/org/20240124145244-laplace_operator.org" "c:/Users/j8154/AppData/Roaming/org/20240124145435-nabla_operator.org" "c:/Users/j8154/AppData/Roaming/org/20240124145702-laplace_operator.org" "c:/Users/j8154/AppData/Roaming/org/20240129101647-photoelectric_effect.org" "c:/Users/j8154/AppData/Roaming/org/20240129101845-computon_effect.org" "c:/Users/j8154/AppData/Roaming/org/20240129104411-quantum_mechanics.org" "c:/Users/j8154/AppData/Roaming/org/20240129105239-uncertainty_principle.org" "c:/Users/j8154/AppData/Roaming/org/20240129113710-theory_of_relativity.org" "c:/Users/j8154/AppData/Roaming/org/20240129160339-electron_volt.org" "c:/Users/j8154/AppData/Roaming/org/20240129160522-radiation_types.org" "c:/Users/j8154/AppData/Roaming/org/20240129163540-neutrino.org" "c:/Users/j8154/AppData/Roaming/org/20240129170336-most_stable_nuclei.org" "c:/Users/j8154/AppData/Roaming/org/20240129171354-nuclei.org" "c:/Users/j8154/AppData/Roaming/org/20240129171550-radioactive_decay.org" "c:/Users/j8154/AppData/Roaming/org/20240129171839-isotope.org" "c:/Users/j8154/AppData/Roaming/org/20240129173148-uranium.org" "c:/Users/j8154/AppData/Roaming/org/20240130153544-enrichment_of_uranium.org" "c:/Users/j8154/AppData/Roaming/org/20240130155641-heavy_water.org" "c:/Users/j8154/AppData/Roaming/org/20240130161331-production_of_heavy_water.org" "c:/Users/j8154/AppData/Roaming/org/20240130232358-nuclear_reactor_generation_i.org" "c:/Users/j8154/AppData/Roaming/org/20240131094042-nuclear_reactor_generation_ii.org" "c:/Users/j8154/AppData/Roaming/org/20240131094315-pressured_water_reactors_pwrs.org" "c:/Users/j8154/AppData/Roaming/org/20240131100522-lighter_water_reactors.org" "c:/Users/j8154/AppData/Roaming/org/20240131100602-boiling_water_reactors.org" "c:/Users/j8154/AppData/Roaming/org/20240131100915-countries_using_nuclear_energy.org" "c:/Users/j8154/AppData/Roaming/org/20240131102448-nuclear_reactors_generation_iii.org" "c:/Users/j8154/AppData/Roaming/org/20240131110443-nuclear_reactor_generation_iii_plus.org" "c:/Users/j8154/AppData/Roaming/org/20240131110634-evolutionary_power_reactor_epr.org" "c:/Users/j8154/AppData/Roaming/org/20240131110919-advanced_passive_pwr_ap1000.org" "c:/Users/j8154/AppData/Roaming/org/20240131133254-pluoninum.org" "c:/Users/j8154/AppData/Roaming/org/20240131133650-fast_breeder_reactor.org" "c:/Users/j8154/AppData/Roaming/org/20240131140805-nuclear_power_a_very_short_introduction.org" "c:/Users/j8154/AppData/Roaming/org/20240131143731-fissile_material.org" "c:/Users/j8154/AppData/Roaming/org/20240131143822-fissionable_material.org" "c:/Users/j8154/AppData/Roaming/org/20240131150544-nuclear_reactor_generation_iv.org" "c:/Users/j8154/AppData/Roaming/org/20240131151858-very_high_temperature_reactor_vhtr.org" "c:/Users/j8154/AppData/Roaming/org/20240131151931-fast_reactor_fr.org" "c:/Users/j8154/AppData/Roaming/org/20240131154105-supercritical_water_cooled_reactor_scwr.org" "c:/Users/j8154/AppData/Roaming/org/20240131154633-sodium_cooled_fast_reactor_sfr.org" "c:/Users/j8154/AppData/Roaming/org/20240131154659-lead_cooled_fast_reactor_lfr.org" "c:/Users/j8154/AppData/Roaming/org/20240131154729-gas_cooled_fast_reactor_gfr.org" "c:/Users/j8154/AppData/Roaming/org/20240131154939-nuclear_reactor_generations.org" "c:/Users/j8154/AppData/Roaming/org/20240131165147-thorium.org" "c:/Users/j8154/AppData/Roaming/org/20240203203825-nuclear_fuel_reprocessing.org" "c:/Users/j8154/AppData/Roaming/org/20240203204039-radioactive_waste.org" "c:/Users/j8154/AppData/Roaming/org/20240204225507-nuclear_safety.org" "c:/Users/j8154/AppData/Roaming/org/20240204230725-hazards_of_nuclear_power.org" "c:/Users/j8154/AppData/Roaming/org/20240204232801-accident_in_uk_1957_windscale_pile_1.org" "c:/Users/j8154/AppData/Roaming/org/20240205093140-emails.org" "c:/Users/j8154/AppData/Roaming/org/20240205110834-accident_tokyo_electrical_power_company_s_kashiwazaki_kariwa_nuclear_power_station.org" "c:/Users/j8154/AppData/Roaming/org/20240205120400-accident_hamaoka_plant_2009.org" "c:/Users/j8154/AppData/Roaming/org/20240205120818-safety_principles.org" "c:/Users/j8154/AppData/Roaming/org/20240205121535-accident_three_miles_island_1979.org" "c:/Users/j8154/AppData/Roaming/org/20240205121632-accident_chernobyl_1986.org" "c:/Users/j8154/AppData/Roaming/org/20240205150802-accident_fukushima_2011.org" "c:/Users/j8154/AppData/Roaming/org/20240206084344-unit_of_nuclear_safety.org" "c:/Users/j8154/AppData/Roaming/org/20240206111809-advanced_candu_reactor_acr.org" "c:/Users/j8154/AppData/Roaming/org/20240206144724-radiation_harm_mechanism.org" "c:/Users/j8154/AppData/Roaming/org/20240206145325-beta_ray.org" "c:/Users/j8154/AppData/Roaming/org/20240206145350-gamma_ray.org" "c:/Users/j8154/AppData/Roaming/org/20240206145403-alpha_ray.org" "c:/Users/j8154/AppData/Roaming/org/20240206201241-nuclear_fusion.org" "c:/Users/j8154/AppData/Roaming/org/20240206201834-fusion_power_plant.org" "c:/Users/j8154/AppData/Roaming/org/20240206205820-confinement_by_magnetic_fields.org" "c:/Users/j8154/AppData/Roaming/org/20240206205833-confinement_by_inertial_bombardment.org" "c:/Users/j8154/AppData/Roaming/org/20240206210908-tokamak.org" "c:/Users/j8154/AppData/Roaming/org/20240206210919-stellarator.org" "c:/Users/j8154/AppData/Roaming/org/20240206213404-iter.org" "c:/Users/j8154/AppData/Roaming/org/20240212092131-atomic_emission_spectra.org" "c:/Users/j8154/AppData/Roaming/org/20240212094522-energy_level.org" "c:/Users/j8154/AppData/Roaming/org/20240212094548-exited_state.org" "c:/Users/j8154/AppData/Roaming/org/20240212094700-atomic_orbital.org" "c:/Users/j8154/AppData/Roaming/org/20240212104604-double_slit_experiment.org" "c:/Users/j8154/AppData/Roaming/org/20240212105256-glow_discharge.org" "c:/Users/j8154/AppData/Roaming/org/20240212105438-wave_particle_duality.org" "c:/Users/j8154/AppData/Roaming/org/20240212105902-wave_model.org" "c:/Users/j8154/AppData/Roaming/org/20240212113515-planck_einstein_relation.org" "c:/Users/j8154/AppData/Roaming/org/20240213114402-fourier_transform.org" "c:/Users/j8154/AppData/Roaming/org/20240213144412-wave_function.org" "c:/Users/j8154/AppData/Roaming/org/20240213144434-wave_function.org" "c:/Users/j8154/AppData/Roaming/org/20240213144536-maxwell_s_equations.org" "c:/Users/j8154/AppData/Roaming/org/20240213144851-euler_s_formula.org" "c:/Users/j8154/AppData/Roaming/org/20240213144935-divergence.org" "c:/Users/j8154/AppData/Roaming/org/20240213144948-curl.org" "c:/Users/j8154/AppData/Roaming/org/20240213145130-light_polarization.org" "c:/Users/j8154/AppData/Roaming/org/20240213173455-entry.org" "c:/Users/j8154/AppData/Roaming/org/20240214090504-presentations.org" "c:/Users/j8154/AppData/Roaming/org/20240214090542-kick_off_meeting_2024_02_14.org" "c:/Users/j8154/AppData/Roaming/org/20240214110441-molten_salt_reactor_msr.org" "c:/Users/j8154/AppData/Roaming/org/20240214153323-voiding_effects.org" "c:/Users/j8154/AppData/Roaming/org/20240215133801-void_coefficient.org" "c:/Users/j8154/AppData/Roaming/org/20240215145352-doppler_coefficient.org" "c:/Users/j8154/AppData/Roaming/org/20240215150057-moderator_temperature_coefficient_mtc.org" "c:/Users/j8154/AppData/Roaming/org/20240216101917-introduction_to_nuclear_engineering.org" "c:/Users/j8154/AppData/Roaming/org/20240216131454-candu_reactors.org" "c:/Users/j8154/AppData/Roaming/org/20240218005525-nuclear_cross_section.org" "c:/Users/j8154/AppData/Roaming/org/20240219223132-introduction_to_nuclear_reactor_experiments.org" "c:/Users/j8154/AppData/Roaming/org/20240219223441-thermodynamics_in_nuclear_power_plant_systems.org" "c:/Users/j8154/AppData/Roaming/org/20240219224115-conversion_ratio.org" "c:/Users/j8154/AppData/Roaming/org/20240219224919-introduction_to_nuclear_engineering.org" "c:/Users/j8154/AppData/Roaming/org/20240220001436-two_phase_flow.org" "c:/Users/j8154/AppData/Roaming/org/20240220004557-a_series_of_monographs_and_textbooks.org" "c:/Users/j8154/AppData/Roaming/org/20240220103838-the_fourth_generation_of_nuclear_reactors_fundamentals_types_and_benefits_explained.org" "c:/Users/j8154/AppData/Roaming/org/20240220110805-doppler_effect.org" "c:/Users/j8154/AppData/Roaming/org/20240224232458-meetings.org" "c:/Users/j8154/AppData/Roaming/org/20240224232536-kick_off_meeting_23022024.org" "c:/Users/j8154/AppData/Roaming/org/20240225230356-plan_2024.org" "c:/Users/j8154/AppData/Roaming/org/20240226124119-criticality.org" "c:/Users/j8154/AppData/Roaming/org/20240226155504-presentation_know_how.org" "c:/Users/j8154/AppData/Roaming/org/20240227161857-validation_of_the_sodium_void_reactivity_effect_prediction_using_jeff_3_1_nuclear_data.org" "c:/Users/j8154/AppData/Roaming/org/20240227161920-validation_of_the_sodium_void_reactivity_effect_prediction_using_jeff_3_1_nuclear_data.org" "c:/Users/j8154/AppData/Roaming/org/20240227161955-introduction_to_nuclear_engineering.org" "c:/Users/j8154/AppData/Roaming/org/20240227163113-introduction_to_nuclear_engineering.org" "c:/Users/j8154/AppData/Roaming/org/20240228101248-report_one_or_two_pages_of_the_motivation.org" "c:/Users/j8154/AppData/Roaming/org/20240229125311-technology_roadmap_update_for_generation_iv_nuclear_energy_systems.org" "c:/Users/j8154/AppData/Roaming/org/20240302015000-overview_on_lead_cooled_fast_reactor_design_and_related_technologies_development_in_enea.org" "c:/Users/j8154/AppData/Roaming/org/20240302021525-two_dimensional_numerical_simulation_of_single_bubble_rising_behavior_in_liquid_metal_using_moving_particle_semi_implicit_method.org" "c:/Users/j8154/AppData/Roaming/org/20240302223520-two_phase_flow_in_sfr.org" "c:/Users/j8154/AppData/Roaming/org/20240303205900-learning_from_phenix_sfr.org" "c:/Users/j8154/AppData/Roaming/org/20240303224936-pool_type_and_loop_type.org" "c:/Users/j8154/AppData/Roaming/org/20240307112731-bubbles_in_sfr.org" "c:/Users/j8154/AppData/Roaming/org/20240307141133-water_bubbles.org" "c:/Users/j8154/AppData/Roaming/org/20240307141249-sodium_bubbles.org" "c:/Users/j8154/AppData/Roaming/org/20240307152442-satzung_zur_sicherung_guter_wissenschaftlicher_praxis_zur_vermeidung_wissenschaftlichen_fehlverhaltens_und_f_ur_den_umgang_mit_verst_o_ssen.org" "c:/Users/j8154/AppData/Roaming/org/20240307154834-rimanus_meeting_20240308.org" "c:/Users/j8154/AppData/Roaming/org/20240307154931-core_disruptive_accident_cda.org" "c:/Users/j8154/AppData/Roaming/org/20240307173747-galinstan_gainsn.org" "c:/Users/j8154/AppData/Roaming/org/20240307203858-bubble_detection_in_liquid_metal_by_perturbation_of_eddy_currents_model_and_experiments.org" "c:/Users/j8154/AppData/Roaming/org/20240307211653-literature_notes_orb.org" "c:/Users/j8154/AppData/Roaming/org/20240307214219-nuclear_power_a_very_short_introduction.org" "c:/Users/j8154/AppData/Roaming/org/20240309141605-lorenz_force.org" "c:/Users/j8154/AppData/Roaming/org/20240309150151-eddy_current_flow_meter_ecfm.org" "c:/Users/j8154/AppData/Roaming/org/Heindel.2011.org" "c:/Users/j8154/AppData/Roaming/org/literature_notes.org")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
